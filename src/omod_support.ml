(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Omod.Private

let () = (* Redo that here we dont have Unix in Omod *)
  Fmt.ansi_tty := begin
    let rec isatty fd = try Unix.isatty fd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> isatty fd
    | Unix.Unix_error (e, _, _) -> false
    in
    if not (isatty Unix.stdout) then false else
    match Sys.getenv "TERM" with
    | exception Not_found -> false
    | "" | "dumb" -> false
    | _ -> true
  end

let strf = Format.asprintf

module Codec = struct
  type 'a t = { id : string }
  let v ~id = { id }
  let magic = Printf.sprintf "omod-%%VERSION%%-ocaml-%s-" Sys.ocaml_version
  let magic c = magic ^ c.id
  let write : type a. a t -> Omod.fpath -> a -> (unit, string) result =
    fun c f v ->
      File.with_open_out f @@ fun oc ->
      output_string oc (magic c);
      output_value oc v;
      flush oc

  let read : type a. a t -> Omod.fpath -> (a, string) result =
    fun c f ->
      try
        File.with_open_in f @@ fun ic ->
        let magic = magic c in
        let m = really_input_string ic (String.length magic) in
        match m = magic with
        | true -> (input_value ic : a)
        | false ->
            failwith (strf "%s: invalid magic number %S, expected %S" f m magic)
      with
      | Failure e -> Error e
end

module Digest = struct
  include Digest
  let pp ppf d = Format.pp_print_string ppf (to_hex d)
  let pp_opt ppf = function
  | None -> Fmt.string ppf "--------------------------------"
  | Some d -> pp ppf d
  module Set = Set.Make (Digest)
  module Map = Map.Make (Digest)
end

module Log = struct
  type t = { f : 'a. ('a, Format.formatter, unit) Pervasives.format -> 'a }
  let exe = Filename.basename Sys.executable_name
  let nil = let f fmt = Format.ifprintf Format.std_formatter fmt in { f }
  let std = let f fmt = Format.printf ("%s: " ^^ fmt ^^ "@.") exe in { f }
  let err = let f fmt = Format.eprintf ("%s: " ^^ fmt ^^ "@.") exe in { f }
  let time l label f =
    let start = Sys.time () in
    let r = f () in
    l.f "%s: %g" label (Sys.time () -. start);
    r
end

module Dir = struct
  let rec exists dir =
    try Ok (Unix.((stat dir).st_kind = S_DIR)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
    | Unix.Unix_error (e, _, _) ->
        Error (strf "directory %s exists: %s" dir (Unix.error_message e))

  let create ?(path = true) ?(mode = 0o755) dir =
    let rec mkdir d mode = try Ok (Unix.mkdir d mode) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
    | Unix.Unix_error (e, _, _) ->
        let err = Unix.error_message e in
        if d = dir
        then Error (strf "create directory %s: %s" d err)
        else Error (strf "create directory %s: %s: %s" dir d err)
    in
    let rec create_them dirs = match dirs with
    | [] -> Ok true
    | dir :: dirs ->
        match mkdir dir mode with Error _ as e -> e | Ok () -> create_them dirs
    in
    let rec dirs_to_create p acc = match exists p with
    | Error _ as e -> e
    | Ok true -> Ok acc
    | Ok false -> dirs_to_create (Filename.dirname p) (p :: acc)
    in
    match exists dir with
    | Error _ as e -> e
    | Ok true -> Ok false
    | Ok false ->
        match path with
        | false -> create_them [dir]
        | true ->
            match dirs_to_create dir [] with
            | Error _ as e -> e
            | Ok dirs -> create_them dirs
end

module Cobj = struct
  type pkg_id = string * Omod.fpath
  let pp_pkg_id ppf (id, loc) = Fmt.pf ppf "%s %a" id Fmt.faint loc
  let pkg_compare = Pervasives.compare

  type dep = string * Digest.t option
  let pp_dep ppf (n, d) = Fmt.pf ppf "%a %s" Digest.pp_opt d n

  let spec_of_string s =
    try
      let toplevel_module s =
        match String.index s '.' with
        | exception Not_found -> String.capitalize_ascii s
        | i -> failwith (strf "'%s' is not a toplevel module identifier." s)
      in
      let cut_variants pkg s = match String.cut ~sep:'@' s with
      | None -> pkg, toplevel_module s, []
      | Some (m, vs) -> pkg, toplevel_module s, String.rev_cuts ~sep:'@' vs
      in
      match String.cut ~sep:'.' s with
      | None -> Ok (cut_variants None s)
      | Some (pkg, s) -> Ok (cut_variants (Some pkg) s)
    with
    | Failure msg -> Error msg

  type kind = Cmi | Cmo | Cmx
  let kind_to_string = function Cmi -> "cmi" | Cmo -> "cmo" | Cmx -> "cmx"
  let kind_of_string = function
  | "cmi" -> Some Cmi | "cmo" -> Some Cmo | "cmx" -> Some Cmx | _ -> None

  let exts = (* Defines what gets indexed *)
    String.Set.of_list ["cmi"; "cmo"; "cma"; "cmx"; "cmxa"; ]

  type t =
    { kind : kind; pkg_id : pkg_id; name : string; variant : string;
      iface_digest : Digest.t option; iface_deps : dep list;
      in_archive : bool; path : Omod.fpath; path_loads : t list Lazy.t; }

  let variant_of_path (_, pkg_path) path =
    let rec loop pkg_path p = match Filename.dirname p with
    | "." -> ""
    | up when String.equal pkg_path up ->
        if p = path then "" else
        let v = Filename.basename p in
        let len = String.length v in
        if len > 0 && v.[0] = '@' then String.sub v 1 (len - 1) else v
    | up -> loop pkg_path up
    in
    loop pkg_path path

  let modname_of_path path =
    String.capitalize_ascii @@ fst @@ File.cut_ext @@ Filename.basename path

  let v
      ~kind ~pkg_id ~name ~iface_digest ~iface_deps ~in_archive ~path
      ~path_loads
    =
    let variant = variant_of_path pkg_id path in
    { kind; pkg_id; name; variant; iface_digest; iface_deps; in_archive; path;
      path_loads }

  let kind c = c.kind
  let name c = c.name
  let variant c = c.variant
  let pkg_id c = c.pkg_id
  let iface_digest c = c.iface_digest
  let iface_deps c = c.iface_deps
  let in_archive c = c.in_archive
  let path c = c.path
  let path_loads c = Lazy.force c.path_loads
  let to_dep c = c.name, c.iface_digest
  let is_kind k c = c.kind = k
  let equal c c' = c = c'
  let compare = Pervasives.compare
  let ui_compare o0 o1 =
    let c = String.compare (name o0) (name o1) in
    if c <> 0 then c else
    let c = pkg_compare (pkg_id o0) (pkg_id o1) in
    if c <> 0 then c else
    let c = String.compare (variant o0) (variant o1) in
    if c <> 0 then c else
    String.compare (path o0) (path o1)

  let pp ppf c =
    Fmt.pf ppf "@[<v>%s%s %a@,| kind: %s@,| pkg: %a \
                @,| iface-digest: %a@,| iface-deps: @[<v>%a@]\
                @,| path-loads: @[<v>%a@]@]"
      (name c)
      (if (variant c) <> "" then "@" ^ (variant c) else "")
      Fmt.faint (path c) (kind_to_string @@ kind c)
      pp_pkg_id (pkg_id c) Digest.pp_opt (iface_digest c)
      (Fmt.list pp_dep) (iface_deps c)
      (Fmt.list pp_dep) (List.map to_dep @@ path_loads c)

  let of_dobj (module O : Omod_ocamlc.DOBJ) kind ~pkg_id acc path =
    match O.read path with
    | Error _ as e -> e
    | Ok obj ->
        let name = O.name obj in
        let variant = variant_of_path pkg_id path in
        let iface_digest = Some (O.iface_digest obj) in
        let iface_deps = O.iface_deps obj in
        let rec cobj =
          { kind; name; pkg_id; variant; iface_digest; iface_deps;
            in_archive = false; path; path_loads = lazy [cobj] }
        in
        ignore (path_loads cobj) (* Force lazy for marshaling *);
        Ok (cobj :: acc)

  let of_cma ~pkg_id acc path = match Omod_ocamlc.Cma.read path with
  | Error _ as e -> e
  | Ok cma ->
      let cmos = Omod_ocamlc.Cma.cmos cma in
      let cobj_of_cmo ~pkg_id path path_loads cmo =
        let rec cobj =
          { kind = Cmo; name = Omod_ocamlc.Cmo.name cmo; pkg_id;
            variant = variant_of_path pkg_id path;
            iface_digest = Some (Omod_ocamlc.Cmo.iface_digest cmo);
            iface_deps = Omod_ocamlc.Cmo.iface_deps cmo; path;
            in_archive = true;
            path_loads; }
        in
        cobj
      in
      let rec path_loads =
        lazy (List.map (cobj_of_cmo ~pkg_id path path_loads) cmos)
      in
      let cmos = Lazy.force path_loads in
      Ok (List.fold_left (fun acc cmo -> cmo :: acc) acc cmos)

  let of_cmxa ~pkg_id acc path = match Omod_ocamlc.Cmxa.read path with
  | Error _ as e -> e
  | Ok cmxa ->
      let cmxs = Omod_ocamlc.Cmxa.cmxs cmxa in
      let cobj_of_cmx ~pkg_id path path_loads cmx =
        let rec cobj =
          { kind = Cmx; name = Omod_ocamlc.Cmx.name cmx; pkg_id;
            variant = variant_of_path pkg_id path;
            iface_digest = Some (Omod_ocamlc.Cmx.iface_digest cmx);
            iface_deps = Omod_ocamlc.Cmx.iface_deps cmx; path;
            in_archive = true;
            path_loads; }
        in
        cobj
      in
      let rec path_loads =
        lazy (List.map (cobj_of_cmx ~pkg_id path path_loads) cmxs)
      in
      let cmxs = Lazy.force path_loads in
      Ok (List.fold_left (fun acc cmx -> cmx :: acc) acc cmxs)

  let add_file ~pkg_id acc file =
    let ext = snd @@ File.cut_ext file in
    if not (String.Set.mem ext exts) then Ok acc else match ext with
    | "cmi" -> of_dobj (module Omod_ocamlc.Cmi) Cmi ~pkg_id acc file
    | "cmo" -> of_dobj (module Omod_ocamlc.Cmo) Cmo ~pkg_id acc file
    | "cma" -> of_cma ~pkg_id acc file
    | "cmx" -> of_dobj (module Omod_ocamlc.Cmx) Cmx ~pkg_id acc file
    | "cmxa" -> of_cmxa ~pkg_id acc file
    | _ -> assert false

  (* Indexes and dependency resolvers *)

  module Index = struct
    type cobj = t
    type t =
      { nmap : cobj list String.Map.t; dmap : cobj list Digest.Map.t;
        pmap : cobj list String.Map.t; }

    let empty =
      { nmap = String.Map.empty; dmap = Digest.Map.empty;
        pmap = String.Map.empty }

    let of_cobjs ?(init = empty) cobjs =
      let add_name k v m = match String.Map.find k m with
      | exception Not_found -> String.Map.add k [v] m
      | vs -> String.Map.add k (v :: vs) m
      in
      let add_digest k v m = match Digest.Map.find k m with
      | exception Not_found -> Digest.Map.add k [v] m
      | vs -> Digest.Map.add k (v :: vs) m
      in
      let rec loop nmap dmap pmap = function
      | [] -> { nmap; dmap; pmap }
      | c :: cs ->
          let nmap = add_name (name c) c nmap in
          let dmap = match iface_digest c with
          | None -> dmap
          | Some d -> add_digest d c dmap
          in
          let pmap = add_name (fst (pkg_id c)) c pmap in
          loop nmap dmap pmap cs
      in
      loop init.nmap init.dmap init.pmap cobjs

    let cobjs i =
      let add_pkg _ cobjs acc = List.rev_append cobjs acc in
      String.Map.fold add_pkg i.pmap []

    let cobjs_by_name i = i.nmap
    let cobjs_by_digest i = i.dmap
    let cobjs_by_pkg_name i = i.pmap

    let cobjs_for_mod_name n i = match String.Map.find n i.nmap with
    | exception Not_found -> [] | cobjs -> cobjs

    let cobjs_for_iface_digest d i = match Digest.Map.find d i.dmap with
    | exception Not_found -> [] | cobjs -> cobjs

    let cobjs_for_pkg_name n i = match String.Map.find n i.pmap with
    | exception Not_found -> [] | cobjs -> cobjs

    let cobjs_for_dep (n, d) i = match d with
    | None -> cobjs_for_mod_name n i
    | Some d -> cobjs_for_iface_digest d i

    let cobjs_for_dep_res ~variants ~sat ~kind dep i =
      let sat_obj o = is_kind kind o && sat o in
      let try_select_variants = function
      | ([] | [_]) as l -> l
      | objs when String.Set.is_empty variants -> objs
      | objs -> List.filter (fun o -> String.Set.mem (variant o) variants) objs
      in
      let objs = cobjs_for_dep dep i in
      match List.filter sat_obj objs with
      | [] -> (* Try to look for a cmi file *)
          let sat_cmi o = is_kind Cmi o && sat o in
          try_select_variants @@ List.filter sat_cmi objs
      | objs ->
          match List.filter in_archive objs with
          | [] -> try_select_variants objs
          | ars -> try_select_variants ars (* favour archives *)
  end

  type res = t String.Map.t

  let add_obj res deps acc o =
    (* When we add [o] to the resolution we may add new objects that
       need resolving [deps] because [o] may be part of a library
       archive. *)
    let rec loop res deps = function
    | [] -> (res, deps) :: acc
    | o :: os ->
        match String.Map.find (name o) res with
        | o' ->
            if String.equal (path o) (path o') then loop res deps os else acc
        | exception Not_found ->
            let add_dep acc (n, digest as d) = match String.Map.find n res with
            | exception Not_found -> d :: acc
            | o ->
                match digest, iface_digest o with
                | Some d, Some d' when not (Digest.equal d d') -> raise Exit
                | _ -> acc
            in
            match List.fold_left add_dep [] (iface_deps o) with
            | exception Exit -> acc
            | ds -> loop (String.Map.add (name o) o res) (ds :: deps) os
    in
    loop res deps (path_loads o)

  let add_root_alt acc cobjs =
    let rec loop res deps = function
    | [] -> (res, deps) :: acc
    | o :: os ->
        match add_obj res deps [] o with
        | [] -> acc  (* inconsistent root objects *)
        | [(res, deps)] -> loop res deps os
        | _ -> assert false
    in
    loop String.Map.empty [] cobjs

  let rec resolve_deps = fun ~variants ~sat ~kind idx ~root_alts ->
    let rec finish_next_todo acc todo = match todo with
    | [] ->
        let acc = List.find_all (fun r -> not (String.Map.is_empty r)) acc in
        begin match acc with
        | [] ->
            (* Humpf not very user friendly *)
            Error (strf "No consistent load sequence could be found")
        | acc -> Ok acc
        end
    | (res, deps) :: todo -> loop acc todo res deps
    and loop acc todo res = function
    | ((n, digest as d) :: ds) :: rest ->
        begin match String.Map.find n res with
        | o ->
            begin match digest, iface_digest o with
            | Some d, Some d' when not (Digest.equal d d') ->
                finish_next_todo acc todo
            | _ -> loop acc todo res (ds :: rest)
            end
        | exception Not_found ->
            match Index.cobjs_for_dep_res ~variants ~sat ~kind d idx with
            | [] ->
                Error (strf "Dependency %a cannot be resolved" pp_dep d)
            | objs ->
                match List.fold_left (add_obj res (ds :: rest)) [] objs with
                | [] -> finish_next_todo acc todo
                | (res, deps) :: ress ->
                    let todo = List.rev_append ress todo in
                    loop acc todo res deps
        end
    | [] :: [] -> finish_next_todo (res :: acc) todo
    | [] :: rest -> loop acc todo res rest
    | [] -> assert false
    in
    let todo = List.fold_left add_root_alt [] root_alts in
    finish_next_todo [] todo

  let fold_res res f acc =
    (* Topological sort by depth first exploration of the DAG. *)
    let get res n = match String.Map.find n res with
    | exception Not_found -> invalid_arg (strf "Undefined dep name %s" n)
    | o -> o
    in
    let rec loop seen acc = function
    | (o :: os as l) :: todo ->
        let n = name o in
        begin match String.Set.mem (name o) seen with
        | true -> loop seen acc (os :: todo)
        | false ->
            let seen = String.Set.add n seen in
            let add_dep acc (n, _) = match String.Set.mem n seen with
            | true -> (* early filter *) acc
            | false -> get res n :: acc
            in
            let deps =
              let add_path_loads_as_dep acc l = to_dep l :: acc in
              List.fold_left add_path_loads_as_dep (iface_deps o) (path_loads o)
            in
            match List.fold_left add_dep [] deps with
            | [] (* early filter *) -> loop seen (f o acc) (os :: todo)
            | deps -> loop seen acc (deps :: l :: todo)
        end
    | [] :: (o :: os) :: todo -> loop seen (f o acc) (os :: todo)
    | [] :: ([] :: todo) -> loop seen acc todo
    | [] :: [] -> acc
    | [] -> assert false
    in
    let objs = List.rev_map snd @@ String.Map.bindings res in
    loop String.Set.empty acc (objs :: [])

  let loads ~variants ~sat ~kind idx ~root_alts =
    match resolve_deps ~variants ~sat ~kind idx ~root_alts with
    | Error _ as e -> e
    | Ok ress ->
        let add_path (seen, ps as acc) o =
          let p = path o in
          if String.Set.mem p seen then acc else
          (String.Set.add p seen, p :: ps)
        in
        let add_res acc res =
          let robjs = fold_res res List.cons [] in
          let _, loads = List.fold_left add_path (String.Set.empty, []) robjs in
          loads :: acc
        in
        Ok (List.fold_left add_res [] ress)
end

module Pkg = struct

  (* Packages *)

  type t = Cobj.pkg_id

  let log_file_err ~err file e = err.Log.f "%s: %s" file e

  let of_dir ?(err = Log.err) dir =
    let ocaml_pkg () = match Cmd.read ["ocamlc"; "-where"] with
    | Ok p -> "ocaml", String.trim p
    | Error (c, err) -> failwith (strf "ocaml: exited with [%d]: %s" c err)
    in
    let add_pkg acc sub =
      let file = Filename.concat dir sub in
      try match Sys.is_directory file with
      | false -> acc
      | true when String.equal sub "ocaml" -> acc
      | true -> (sub, file) :: acc
      with Sys_error e -> log_file_err ~err file e; acc
    in
    try
      let pkgs = Array.fold_left add_pkg [] (Sys.readdir dir) in
      let pkgs = ocaml_pkg () :: pkgs in
      List.sort compare @@ pkgs
    with
    | Sys_error e -> log_file_err ~err dir e; []
    | Failure e -> err.Log.f "%s" e; []

  let fold_pkg_files ?(err = Log.err) f acc (pkg_name, dir) =
    let rec loop acc = function
    | [] -> acc
    | d :: dirs ->
        try
          let rec fold_dir acc dirs = function
          | [] -> loop acc dirs
          | base :: fs ->
              let file = Filename.concat d base in
              try match Sys.is_directory file with
              | true -> fold_dir acc (file :: dirs) fs
              | false -> fold_dir (f acc file) dirs fs
              with
              | Sys_error e -> log_file_err ~err file e; fold_dir acc dirs fs
          in
          fold_dir acc dirs (Array.to_list @@ Sys.readdir d)
        with
        | Sys_error e -> log_file_err ~err d e; loop acc dirs
    in
    loop acc [dir]

  let find_cobjs ?(err = Log.err) ?(acc = []) pkg_id =
    let add_file acc file = match Cobj.add_file ~pkg_id acc file with
    | Error e -> err.Log.f "%s" e; acc
    | Ok acc -> acc
    in
    fold_pkg_files ~err add_file acc pkg_id

  let equal = ( = )
  let compare = Cobj.pkg_compare
  let pp = Cobj.pp_pkg_id
  let pp_name ppf (n, _) = Fmt.string ppf n

  module P = struct
    type nonrec t = t
    let equal = equal
    let compare = compare
  end
  module Set = Set.Make (P)
  module Map = Map.Make (P)

  (* Signatures *)

  type signature = Digest.t

  external caml_string_set_64 :
    bytes -> int -> int64 -> unit = "%caml_string_set64"

  let digest_mtimes ?(err = Log.err) paths =
    let mtime_to_string m =
      let b = Bytes.create 8 in
      caml_string_set_64 b 0 (Int64.bits_of_float m);
      Bytes.unsafe_to_string b
    in
    let rec add_mtime acc p =
      try (mtime_to_string @@ (Unix.stat p).Unix.st_mtime) :: acc with
      | Unix.Unix_error (Unix.EINTR, _, _) -> add_mtime acc p
      | Unix.Unix_error (e, _, _) ->
          log_file_err ~err p (Unix.error_message e); acc
    in
    let paths = List.sort String.compare paths in
    let mtimes = List.fold_left add_mtime [] paths in
    Digest.string @@ String.concat "" mtimes

  let signature ?(err = Log.err) pkg
    =
    let add_cobj acc file =
      if String.Set.mem (snd (File.cut_ext file)) Cobj.exts
      then file :: acc else acc
    in
    digest_mtimes ~err @@ fold_pkg_files ~err add_cobj [] pkg

  (* Information *)

  type info = { signature : signature; cobjs : Cobj.t list }
  let info ~signature ~cobjs = { signature; cobjs }
  let info_signature i = i.signature
  let info_cobjs i = i.cobjs

  let pp_info ppf i =
    let pp_cobj ppf c =
      Fmt.pf ppf "@[%a %s %a@]"
        Digest.pp_opt (Cobj.iface_digest c) (Cobj.name c)
        Fmt.faint (Cobj.path c)
    in
    Fmt.pf ppf "@[<v>| signature: %a@,| cobjs:@,  @[<v>%a@]@]"
      Digest.pp i.signature (Fmt.list pp_cobj)
      (List.sort Cobj.ui_compare i.cobjs)

  (* Databases *)

  type db = info Map.t

  let log_progress ~note ~progress pkg =
    if progress then
      note.Log.f "[%a] %a" (Fmt.tty_str ~mode:"32") "INDEX" pp pkg

  let log_indexing ~note =
    note.Log.f "[%a] In progress..." (Fmt.tty_str ~mode:"32") "INDEXING"

  let db ?(err = Log.err) ?(note = Log.nil) ?(progress = false)
      ?(init = Map.empty) pkgs
    =
    let add_pkg acc pkg =
      let signature = signature ~err pkg in
      let cobjs = (log_progress ~note ~progress pkg; find_cobjs ~err pkg) in
      let info = info ~signature ~cobjs in
      Map.add pkg info acc
    in
    log_indexing ~note;
    List.fold_left add_pkg init pkgs

  let db_to_name_db db =
    let add_name (n, _ as id) info acc = String.Map.add n (id, info) acc in
    Map.fold add_name db String.Map.empty

  let db_to_cobj_index db =
    let add_cobjs _ i acc = Cobj.Index.of_cobjs ~init:acc (info_cobjs i) in
    Map.fold add_cobjs db Cobj.Index.empty

  type diff =
  [ `New of t * signature
  | `Changed of t * signature
  | `Gone of t ]

  let pp_diff ppf = function
  | `New (p, _) ->   Fmt.pf ppf "[NEW  ] %a" pp p
  | `Changed (p, _) -> Fmt.pf ppf "[STALE] %a" pp p
  | `Gone p ->  Fmt.pf ppf "[GONE ] %a" pp p

  let diff m state =
    let rec loop acc unseen = function
    | [] ->
        let add_unseen pkg acc = `Gone pkg :: acc in
        List.rev (Set.fold add_unseen unseen acc)
    | (pkg, sgn) :: ps ->
        match Map.find pkg m with
        | exception Not_found -> loop (`New (pkg, sgn) :: acc) unseen ps
        | i ->
            let unseen = Set.remove pkg unseen in
            match Digest.equal (info_signature i) sgn with
            | true -> loop acc unseen ps
            | false -> loop (`Changed (pkg, sgn) :: acc) unseen ps
    in
    let unseen = Map.fold (fun k v acc -> Set.add k acc) m Set.empty in
    loop [] unseen state

  let update ?(err = Log.err) ?(note = Log.nil) ?(progress = false) m ds =
    if ds = [] then m else
    let rec loop m = function
    | [] -> m
    | d :: ds ->
      match d with
      | `Gone pkg -> loop (Map.remove pkg m) ds
      | `New (pkg, signature)
      | `Changed (pkg, signature) ->
          let cobjs = (log_progress ~note ~progress pkg; find_cobjs ~err pkg) in
          let info = info ~signature ~cobjs in
          loop (Map.add pkg info m) ds
    in
    (log_indexing ~note; loop m ds)
end

module Conf = struct
  let get_env k = match Sys.getenv k with
  | "" -> None | exception Not_found -> None | v -> Some v

  let ( / ) = Filename.concat
  let in_prefix_path dir =
    Filename.(dirname @@ dirname Sys.executable_name) / dir

  let libdir_env = "OMOD_LIBDIR"
  let get_libdir libdir = match libdir with
  | Some l -> l | None ->
      match get_env libdir_env with
      | Some l -> l | None -> in_prefix_path "lib"

  let cache_env = "OMOD_CACHE"
  let get_cache cache = match cache with
  | Some l -> l | None ->
      match get_env cache_env with
      | Some l -> l | None -> in_prefix_path ("var" / "cache" / "omod")

  type t = { cache : Omod.fpath; libdir : Omod.fpath }

  let v ?cache ?libdir () =
    try
      let cache = get_cache cache in
      let libdir = get_libdir libdir in
      Ok { cache; libdir; }

    with
    | Failure e -> Error e

  let cache c = c.cache
  let libdir c = c.libdir
  let pp ppf c = Fmt.pf ppf "@[<v>cache: %s@,libdir: %s@]" c.cache c.libdir
end

module Cache = struct
  type t = { pkgs : Pkg.db; }
  let v ~pkgs = { pkgs }
  let pkgs c = c.pkgs
  let file conf = Filename.concat (Conf.cache conf) "cache"
  let codec = Codec.v ~id:"cache"
  let read conf ~force ~err =
    let file = file conf in
    match File.exists file with
    | Error _ as e -> e
    | Ok false -> Ok None
    | Ok true ->
        match Codec.read codec file with
        | Error m as e -> if force then (err.Log.f "%s" m; Ok None) else e
        | Ok c -> Ok (Some c)

  let write conf c = match Dir.create ~path:true (Conf.cache conf) with
  | Error _ as e -> e
  | Ok _ -> Codec.write codec (file conf) c

  let clear conf = match File.delete (file conf) with
  | Error _ as e -> e | Ok _ -> Ok ()

  let _refresh ~err ~note ~progress conf = function
  | None ->
      let pkgs = Pkg.of_dir ~err (Conf.libdir conf) in
      let db = Pkg.db ~err ~note ~progress pkgs in
      let c = { pkgs = db } in
      (match write conf c with | Error _ as e -> e | Ok () -> Ok c)
  | Some c ->
      let pkgs = Pkg.of_dir ~err (Conf.libdir conf) in
      let sigs = List.rev_map (fun p -> (p, Pkg.signature ~err p)) pkgs in
      match Pkg.diff c.pkgs sigs with
      | [] -> Ok c
      | diffs ->
          let pkgs = Pkg.update ~err ~note ~progress c.pkgs diffs in
          let c = { pkgs } in
          (match write conf c with | Error _ as e -> e | Ok () -> Ok c)

  let get ?(err = Log.err) ?(note = Log.nil) ?(progress = false)
      conf ~force ~trust
    =
    match read conf ~force ~err with
    | Error e when not force ->
        (* A bit ugly but let's make this easy *)
        Error (e ^ "\nTry to run with '-f' to force the cache")
    | Error _ as e -> e
    | Ok c when not trust -> _refresh ~err ~note ~progress conf c
    | Ok (Some c) -> Ok c
    | Ok None ->
        if force then _refresh ~err ~note ~progress conf None else
        Error (strf "Cannot trust cache, %s does not exist" (file conf))

  let status ?(err = Log.err) conf c =
    let pkgs = Pkg.of_dir ~err (Conf.libdir conf) in
    let sigs = List.map (fun p -> (p, Pkg.signature ~err p)) pkgs in
    match c with
    | Some c -> Pkg.diff c.pkgs sigs
    | None -> List.rev (List.rev_map (fun (p, sg) -> `New (p, sg)) sigs)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
