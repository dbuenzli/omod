(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Omod.Private
open Omod_support

(* Formatting and error handling *)

let strf = Format.asprintf
let log_err = Log.(err.f)

let handle_cache_error v f = match v with
|  Error e -> log_err "%s" e; 2 | Ok v -> f v

let handle_name_error v f = match v with
| Error e -> log_err "%s" e; 1 | Ok v -> f v

let did_you_mean ?(pre = "Unknown") ?(post = "") ~kind (n, hints) =
  match hints with
  | [] -> strf "@[%s %s '%s'%s.@]" pre kind n post
  | hints ->
      strf "@[%s %s '%s'%s.@ Did you mean %a ?@]"
        pre kind n post Format.pp_print_text (Cmdliner.Arg.doc_alts hints)

(* Lookups *)

let has_kind kinds = match kinds with
| [] -> fun _ -> true
| ks -> fun o -> List.mem (Cobj.kind o) kinds

let has_pkg pkgs = match String.Set.is_empty pkgs with
| true -> fun _ -> true
| false -> fun o -> String.Set.mem (fst (Cobj.pkg_id o)) pkgs

let has_variant variants = match String.Set.is_empty variants with
| true -> fun _ -> true
| false -> fun o ->
  (Cobj.variant o) = "" || String.Set.mem (Cobj.variant o) variants

let kind_names_exist ?post ~kind dom names =
  let rec loop miss = function
  | n :: ns when String.Map.mem n dom -> loop miss ns
  | n :: ns -> loop (n :: miss) ns
  | [] ->
      if miss = [] then Ok () else
      let existing = List.rev_map fst (String.Map.bindings dom) in
      let add_error acc n =
        did_you_mean ?post ~kind (n, String.suggest existing n) :: acc
      in
      Error (String.concat "\n" (List.fold_left add_error [] miss))
  in
  loop [] names

let pkg_names_exist idx names =
  kind_names_exist ~kind:"package" (Cobj.Index.cobjs_by_pkg_name idx) names

let mod_names_exist idx pkgs names =
  let idx, post = match pkgs with
  | [] -> idx, ""
  | pkgs ->
      (* Restrict search to packages *)
      let in_package = match pkgs with
      | [pkg] -> " in package " ^ Filename.quote pkg
      | pkgs ->
          "in package(s)" ^ String.concat ", " (List.map Filename.quote pkgs)
      in
      let add_pkg acc p =
        Cobj.Index.of_cobjs ~init:acc (Cobj.Index.cobjs_for_pkg_name p idx)
      in
      List.fold_left add_pkg Cobj.Index.empty pkgs, in_package
  in
  kind_names_exist
    ~kind:"module name" ~post (Cobj.Index.cobjs_by_name idx) names

let err_not_found idx pkgs mod_name =
  let pkgs = String.Set.elements pkgs in
  match pkg_names_exist idx pkgs with
  | Error _ as e -> e
  | Ok () ->
      match mod_names_exist idx pkgs [mod_name] with
      | Error _ as e -> e | Ok _ -> assert false

let find_cobjs_by_spec idx pkgs mod_name kinds =
  let pkgs, name, variants = match Cobj.spec_of_string mod_name with
  | None, n, vs -> String.Set.of_list pkgs, n, String.Set.of_list vs
  | Some p, n, vs -> String.Set.of_list [p], n, String.Set.of_list vs
  in
  let has_kind = has_kind kinds in
  let has_pkg = has_pkg pkgs in
  let has_variant = has_variant variants in
  match Cobj.Index.cobjs_for_mod_name name idx with
  | [] -> err_not_found idx pkgs name
  | objs ->
      match List.filter has_pkg objs with
      | [] -> err_not_found idx pkgs name
      | objs ->
          match List.filter has_variant objs with
          | [] -> Ok (List.filter has_kind objs)
          | objs -> Ok (List.filter has_kind objs)

let find_cobjs_by_specs idx pkgs mod_names kinds =
  let rec loop acc = function
  | [] -> Ok acc
  | n :: ns ->
      match find_cobjs_by_spec idx pkgs n kinds with
      | Error _ as e -> e
      | Ok cobjs -> loop (List.rev_append cobjs acc) ns
  in
  if mod_names <> [] then loop [] mod_names else
  match pkg_names_exist idx pkgs with
  | Error _ as e -> e
  | Ok () ->
      let has_kind = has_kind kinds in
      let has_pkg = has_pkg (String.Set.of_list pkgs) in
      let add_cobjs k cobjs acc =
        let cobjs = List.filter has_kind cobjs in
        let cobjs = List.filter has_pkg cobjs in
        List.rev_append cobjs acc
      in
      Ok (String.Map.fold add_cobjs (Cobj.Index.cobjs_by_name idx) [])

let get_cache ?(err = Log.err) ?(note = Log.err) conf ~quiet ~force ~trust =
  let note = if quiet then Log.nil else note in
  let err = if quiet then Log.nil else err in
  Cache.get ~err ~note conf ~trust ~force

(* cache command *)

let cache_cmd conf action quiet force = match action with
| `Path -> Printf.printf "%s\n%!" (Cache.file conf); 0
| `Clear -> handle_cache_error (Cache.clear conf) @@ fun _ -> 0
| `Refresh ->
    handle_cache_error
      (get_cache conf ~err:Log.err ~note:Log.std ~quiet ~force ~trust:false)
    @@ fun _ -> 0
| `Status ->
    let err = if quiet then Log.nil else Log.err in
    handle_cache_error (Cache.read conf ~force ~err) @@ fun c ->
    match Cache.status conf c ~err with
    | [] -> 0
    | st -> Format.printf "@[<v>%a@]@." (Fmt.list Pkg.pp_diff) st; 0

(* cobj command *)

let pp_cobj = function
| `Short ->
    fun ppf c ->
      Fmt.pf ppf "@[%s %a@]" (Cobj.name c) Digest.pp_opt (Cobj.iface_digest c)
| `Normal ->
    fun ppf c ->
      Fmt.pf ppf "@[%s %a %a@]"
        (Cobj.name c) Digest.pp_opt (Cobj.iface_digest c)
        Fmt.faint (Cobj.path c)
| `Long ->
    fun ppf c -> Fmt.pf ppf "%a@," Cobj.pp c

let cobj_cmd (conf, cache) out_fmt pkg_names mod_names cobj_kinds =
  handle_cache_error cache @@ fun cache ->
  let idx = Pkg.db_to_cobj_index (Cache.pkgs cache) in
  handle_name_error (find_cobjs_by_specs idx pkg_names mod_names cobj_kinds) @@
  fun cobjs ->
  let cobjs = List.sort Cobj.ui_compare cobjs in
  Format.printf "@[<v>%a@]@." (Fmt.list (pp_cobj out_fmt)) cobjs; 0

(* conf command *)

let conf_cmd c = Format.printf "%a@." Conf.pp c; 0

(* list command *)

let pp_cmi = function
| `Short -> fun ppf c -> Fmt.pf ppf "%s" (Cobj.name c)
| `Long -> fun ppf c -> Fmt.pf ppf "%a@," Cobj.pp c
| `Normal ->
    fun ppf c -> Fmt.pf ppf "@[%s %a@]" (Cobj.name c) Fmt.faint (Cobj.path c)

let list_cmd (conf, cache) out_fmt pkg_names mod_names =
  handle_cache_error cache @@ fun cache ->
  let idx = Pkg.db_to_cobj_index (Cache.pkgs cache) in
  handle_name_error (find_cobjs_by_specs idx pkg_names mod_names [Cobj.Cmi]) @@
  fun cobjs ->
  let cobjs = List.sort Cobj.ui_compare cobjs in
  Format.printf "@[<v>%a@]@." (Fmt.list (pp_cmi out_fmt)) cobjs; 0

(* load command *)

let is_vmthreads_variant o = Cobj.variant o = "vmthreads"
let is_profile_obj o = (* m.p.ext files (mainly in stdlib) *)
  let p = Cobj.path o in
  match String.rindex p Filename.dir_sep.[0] with
  | exception Not_found -> false
  | first_sep ->
      match String.rindex p '.' with
      | exception Not_found -> false
      | i ->
          match String.rindex_from p (i - 1) '.' with
          | exception Not_found -> false
          | j -> first_sep < j && (i - j) = 2 && p.[j + 1] = 'p'

let load idx nat mod_name =
  let kinds = Cobj.Cmi :: if nat then [Cobj.Cmx] else [Cobj.Cmo] in
  match find_cobjs_by_spec idx [] mod_name kinds with
  | Error _ as e -> e
  | Ok _ ->
      let _, m, variants = Cobj.spec_of_string mod_name in
      let variants = String.Set.of_list variants in
      let kind = if nat then Cobj.Cmx else Cobj.Cmo in
      let sat o = not (is_vmthreads_variant o) && not (is_profile_obj o) in
      Cobj.loads ~variants ~sat ~kind idx ~roots:[(m, None)]

let load_cmd (conf, cache) nat mod_name =
  handle_cache_error cache @@ fun cache ->
  let idx = Pkg.db_to_cobj_index (Cache.pkgs cache) in
  match load idx nat mod_name with
  | Error e -> Format.printf "@[<v>error:@,%s@,@]" e; 0
  | Ok loads ->
      let pp_load = match nat with
      | false -> Format.pp_print_string
      | true ->
          fun ppf s -> match File.cut_ext s with
          | (f, ("cmxa"|"cmx")) -> Format.fprintf ppf "%s.cmxs" f
          | _ -> Format.pp_print_string ppf s
      in
      let pp_loads ppf ls =
        Fmt.pf ppf "@[<v>load:@,%a@]" (Fmt.list pp_load) ls
      in
      Format.printf "@[<v>%a@]@." (Fmt.list pp_loads) loads;
      0

(* check command *)

let check_cmd (conf, cache) pkg_names nat =
  handle_cache_error cache @@ fun cache ->
  let idx = Pkg.db_to_cobj_index (Cache.pkgs cache) in
  handle_name_error (find_cobjs_by_specs idx pkg_names [] [Cobj.Cmi]) @@
  fun cobjs ->
  let try_load acc cobj = match load idx nat (Cobj.name cobj) with
  | Error e -> Format.printf "@[%s: %s@]@." (Cobj.name cobj) e; 3
  | Ok _ -> acc
  in
  List.fold_left try_load 0 cobjs

(* pkg command *)

let pp_pkg = function
| `Short -> fun ppf (p, i) -> Pkg.pp_name ppf p
| `Normal -> fun ppf (p, i) -> Pkg.pp ppf p
| `Long -> fun ppf (p, i) -> Fmt.pf ppf "@[<v>%a@,%a@,@]" Pkg.pp p Pkg.pp_info i

let find_pkgs db = function
| [] -> Ok (Pkg.Map.bindings db)
| ns ->
    let ndb = Pkg.db_to_name_db db in
    let add_pkg (fnd, miss) n = match String.Map.find n ndb with
    | exception Not_found -> (fnd, n :: miss)
    | pkg -> (pkg :: fnd, miss)
    in
    let fnd, miss = List.fold_left add_pkg ([],[]) ns in
    match miss with
    | [] -> Ok fnd
    | miss ->
        let exists = List.rev_map fst (String.Map.bindings ndb) in
        let add_error acc n =
          did_you_mean ~kind:"package" (n, String.suggest exists n) :: acc
        in
        Error (String.concat "\n" (List.fold_left add_error [] miss))

let pkg_cmd (conf, cache) out_fmt pkg_names =
  handle_cache_error cache @@ fun cache ->
  handle_name_error (find_pkgs (Cache.pkgs cache) pkg_names) @@ fun pkgs ->
  Format.printf "@[<v>%a@]@." (Fmt.list (pp_pkg out_fmt)) pkgs; 0

(* Command line interface *)

open Cmdliner

let exits =
  Term.exit_info 1 ~doc:"a specified entity name cannot be found" ::
  Term.exit_info 2 ~doc:"a cache error occured" ::
  Term.default_exits

(* Arguments *)

type out_fmt = [ `Normal | `Short | `Long ]
let out_fmt =
  let short =
    Arg.info ["short"]
      ~doc:"Short output. Line based output with only relevant data."
  in
  let long =
    Arg.info ["long"]
      ~doc:"Long output. Outputs as much information as possible."
  in
  Arg.(value & vflag `Normal [`Short, short; `Long, long])

let trust =
  let doc = "Trust the cache to be up-to-date." in
  let env = Arg.env_var "OMOD_TRUST_CACHE" in
  Arg.(value & flag & info ["t"; "trust-cache"] ~doc ~env)

let force =
  let doc = "Force rebuilding the cache in case of error." in
  let env = Arg.env_var "OMOD_FORCE_CACHE" in
  Arg.(value & flag & info ["f"; "force"] ~doc ~env)

let quiet =
  let doc = "Be quiet." in
  Arg.(value & flag & info ["q"; "quiet"] ~doc)

let conf =
  let libdir =
    let doc = "Library directory, $(b,opam var lib) if unspecified." in
    let docv = "PATH" in
    let env = Arg.env_var Conf.libdir_env in
    Arg.(value & opt (some string) None & info ["libdir"] ~doc ~env ~docv)
  in
  let cache =
    let doc = "Cache directory, $(b,opam var prefix)/var/cache/omod if \
               unspecified."
    in
    let env = Arg.env_var Conf.cache_env in
    Arg.(value & opt (some string) None & info ["cache"] ~doc ~env ~docv:"PATH")
  in
  let conf libdir cache = match Conf.v ?libdir ?cache () with
  | Error e -> `Error (false, e)
  | Ok v -> `Ok v
  in
  Term.(ret @@ (const conf $ libdir $ cache))

let cache =
  let cache conf quiet force trust =
    conf, get_cache conf ~quiet ~force ~trust
  in
  Term.(const cache $ conf $ quiet $ force $ trust)

let pkgs_pos =
  let doc = "Package to consider (repeatable)." in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"PKG")

let pkgs_opt =
  let doc = "Package to consider (repetable)." in
  Arg.(value & opt_all string [] & info ["p"; "pkg"] ~doc ~docv:"PKG")

let mod_docv = "[PKG.]MOD(@VARIANT)*"
let mod_names =
  let doc = "Module names to consider (repeatable)." in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:mod_docv)

let mod_name =
  let doc = "Module name" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:mod_docv)

let cobj_kinds =
  let doc = "Compilation object kind to consider (repeatable)" in
  let kind =
    let parse s = match Cobj.kind_of_string s with
    | Some k -> Ok k
    | None ->
        Error (`Msg (strf "%S: could not parse compilation object kind" s))
    in
    let pp ppf k = Fmt.string ppf (Cobj.kind_to_string k) in
    Arg.conv ~docv:"KIND" (parse, pp)
  in
  Arg.(value & opt_all kind [] & info ["k"; "kind"] ~doc)

let nat =
  let doc = "cmx based load sequence for ocamlnat." in
  Arg.(value & flag & info ["n"; "nat"] ~doc)

(* Command clis *)

let cache_cmd =
  let doc = "Operate on the omod cache" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(tname) $(i,ACTION) [$(i,OPTION)]...";
    `S Manpage.s_description;
    `P "The $(tname) command operates on the omod cache. See the available
        actions below.";
    `S "ACTIONS";
    `I ("$(b,path)", "Display the path to the cache");
    `I ("$(b,clear)", "Clear the cache");
    `I ("$(b,refresh)", "Refresh the cache");
    `I ("$(b,status)", "Display cache status"); ]
  in
  let action =
    let action =
      [ "path", `Path; "clear", `Clear; "refresh", `Refresh; "status", `Status]
    in
    let doc = strf "The action to perform. $(docv) must be one of %s."
        (Arg.doc_alts_enum action)
    in
    let action = Arg.enum action in
    Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")
  in
  Term.(const cache_cmd $ conf $ action $ quiet $ force),
  Term.info "cache" ~doc ~exits ~man ~man_xrefs

let cobj_cmd =
  let doc = "Show compilation objects" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) shows compilation objects known to omod. If no name or
        package restriction is specified, all objects are shown."; ]
  in
  Term.(const cobj_cmd $ cache $ out_fmt $ pkgs_opt $ mod_names $ cobj_kinds),
  Term.info "cobj" ~doc ~exits ~man ~man_xrefs

let conf_cmd =
  let doc = "Show omod configuration" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs the omod configuration.";
    `P "$(mname) needs to know the path to the OCaml library directory and
        and the path to the omod cache. Each of these two can be
        specified on the command line or via an environment variable.
        If none of this is done they are discovered via $(b,opam(1\\)), see the
        options $(b,--libdir) and $(b,--cache)." ]
  in
  Term.(const conf_cmd $ conf),
  Term.info "conf" ~doc ~exits ~man ~man_xrefs

let check_cmd =
  let doc = "Check accessible modules can be loaded" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) reports accessible modules (see $(b,omod list)) which
        cannot be loaded."; ]
  in
  let exits =
    Term.exit_info 3 ~doc:"some accessible modules cannot be loaded" :: exits
  in
  Term.(const check_cmd $ cache $ pkgs_opt $ nat),
  Term.info "check" ~doc ~exits ~man ~man_xrefs

let list_cmd =
  let doc = "Show accessible modules (default command)" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) lists the accessible modules names. These are
        the names of cmi files installed." ]
  in
  Term.(const list_cmd $ cache $ out_fmt $ pkgs_opt $ mod_names),
  Term.info "list" ~doc ~exits ~man ~man_xrefs

let load_cmd =
  let doc = "Show module load sequence" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) shows the load sequence of a module. This command
        does not exit with 1 in case the module is unknown it reports
        the error on stdout in machine readable format."; ]
  in
  Term.(const load_cmd $ cache $ nat $ mod_name),
  Term.info "load" ~doc ~exits ~man ~man_xrefs

let pkg_cmd =
  let doc = "Show packages" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows packages known to omod. If no packages
        are specified, all packages are shown." ]
  in
  Term.(const pkg_cmd $ cache $ out_fmt $ pkgs_pos),
  Term.info "pkg" ~doc ~exits ~man ~man_xrefs

(* Main command *)

let omod =
  let doc = "Lookup installed OCaml modules" in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) looks up installed OCaml modules. It provides a mecanism
        to load modules and their dependencies in the OCaml toplevel system
        (REPL). See $(b,odig doc omod) for more details.";
    `P "See $(mname) $(b,conf) for information about $(mname)
        configuration."; ]
  in
  fst list_cmd,
  Term.info "omod" ~version:"%%VERSION%%" ~doc ~exits ~man

let () =
  let cmds = [cache_cmd; check_cmd; cobj_cmd; conf_cmd; list_cmd; load_cmd;
              pkg_cmd]
  in
  Term.(exit_status @@ eval_choice omod cmds)

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
