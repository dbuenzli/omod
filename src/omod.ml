(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Preliminaries *)

let strf = Printf.sprintf
type fpath = string

module String = struct
  include String

  let cut ~sep s = match String.index s sep with
  | exception Not_found -> None
  | i -> String.(Some (sub s 0 i, sub s (i + 1) (length s - (i + 1))))

  let rev_cut ~sep s = match String.rindex s sep with
  | exception Not_found -> None
  | i -> String.(Some (sub s 0 i, sub s (i + 1) (length s - (i + 1))))

  let rev_cuts ~sep s =
    let rec loop acc = function
    | "" -> acc
    | s ->
        match rev_cut ~sep s with
        | None -> s :: acc
        | Some (l, r) -> loop (r :: acc) l
    in
    loop [] s

  let starts_with ~prefix s = (* once 4.13 is requird this can be removed. *)
    let len_a = length prefix in
    let len_s = length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let rec loop i =
      if i > max_idx_a then true else
      if unsafe_get prefix i <> unsafe_get s i then false else loop (i + 1)
    in
    loop 0

  (* Suggesting *)

  let edit_distance s0 s1 =
    (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
    let minimum a b c = min a (min b c) in
    let m = String.length s0 in
    let n = String.length s1 in
    (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
       the first i characters of s and the first j characters of t *)
    let d = Array.make_matrix (m+1) (n+1) 0 in
    for i = 0 to m do d.(i).(0) <- i done;
    for j = 0 to n do d.(0).(j) <- j done;
    for j = 1 to n do
      for i = 1 to m do
        if s0.[i-1] = s1.[j-1]
        then d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
        else
        d.(i).(j) <- minimum
            (d.(i-1).(j) + 1)   (* a deletion *)
            (d.(i).(j-1) + 1)   (* an insertion *)
            (d.(i-1).(j-1) + 1) (* a substitution *)
      done;
    done;
    d.(m).(n)

  let suggest ?(dist = 2) candidates s =
    let add (min, acc) name =
      let d = edit_distance s name in
      if d = min then min, (name :: acc) else
      if d < min then d, [name] else
      min, acc
    in
    let d, suggs = List.fold_left add (max_int, []) candidates in
    if d <= dist (* suggest only if not too far *) then List.rev suggs else []

  module Set = Set.Make (String)
  module Map = Map.Make (String)
end

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit

  let ansi_tty =
    (* Best effort, we are missing Unix.isatty. Redefined by Omod_support. *)
    ref begin match Sys.getenv "TERM" with
    | exception Not_found -> false
    | "" | "dumb" -> false
    | _ -> true
    end

  let pf = Format.fprintf
  let string = Format.pp_print_string
  let list ?sep:pp_sep = Format.pp_print_list ?pp_sep
  let tty_str ~mode ppf s = match !ansi_tty with
  | false -> string ppf s
  | true -> pf ppf "@<0>%s%s@<0>%s" (Printf.sprintf "\027[%sm" mode) s "\027[m"

  let faint = tty_str ~mode:"02"
end

module File = struct
  let cut_ext f = match String.rindex f '.' with
  | exception Not_found -> f, ""
  | i ->
      let j = match String.rindex f Filename.dir_sep.[0] with
      | exception Not_found -> -1 | j -> j
      in
      if j > i then f, "" else
      String.(sub f 0 i, sub f (i + 1) (length f - (i + 1)))

  let catch_sys_error fn = try fn () with Sys_error e -> Error e
  let with_io_chan close file chan fn =
    try let r = fn chan in close chan; Ok r with
    | e ->
        (try ignore (close chan) with Sys_error _ -> ());
        match e with
        | Sys_error err -> Error (strf "%s: %s" file err)
        | End_of_file -> Error (strf "%s: unexpected end of file" file)
        | e -> raise e

  let with_open_in file fn =
    catch_sys_error @@ fun () ->
    let ic = open_in_bin file in
    with_io_chan close_in file ic fn

  let with_open_out file fn =
    catch_sys_error @@ fun () ->
    let oc = open_out_bin file in
    with_io_chan close_out file oc fn

  let to_string file =
    with_open_in file @@ fun ic ->
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Bytes.unsafe_to_string buf

  let of_string file s =
    with_open_out file @@ fun oc ->
    output_string oc s

  let exists file = catch_sys_error @@ fun () ->
    Ok (Sys.file_exists file && not (Sys.is_directory file))

  let delete file = catch_sys_error @@ fun () ->
    let exists = Sys.file_exists file in
    (if exists then Sys.remove file else ());
    Ok exists
end

module Cmd = struct
  type t = string list

  let find cmds =
    let test, null = match Sys.os_type with
    | "Win32" -> "where", " NUL"
    | _ -> "command -v", "/dev/null"
    in
    let cmd c = Sys.command (strf "%s %s 1>%s 2>%s" test c null null) = 0 in
    try Some (List.find cmd cmds) with Not_found -> None

  let err_cmd exit cmd = Error (exit, strf "exited with %d: %s\n" exit cmd)
  let quote_cmd = match Sys.win32 with
  | false -> fun cmd -> cmd
  | true -> fun cmd -> strf "\"%s\"" cmd

  let run args =
    let cmd = String.concat " " (List.map Filename.quote args) in
    let exit = Sys.command (quote_cmd cmd) in
    if exit = 0 then Ok () else err_cmd exit cmd

  let read args =
    let stdout = Filename.temp_file (Filename.basename Sys.argv.(0)) "omod" in
    at_exit (fun () -> try ignore (Sys.remove stdout) with Sys_error _ -> ());
    let cmd = String.concat " " (List.map Filename.quote args) in
    let cmd = quote_cmd @@ strf "%s 1>%s" cmd (Filename.quote stdout) in
    let exit = Sys.command cmd in
    if not (exit = 0) then err_cmd exit cmd else
    match File.to_string stdout with
    | Error e -> Error (exit, e)
    | Ok _ as v -> v
end

(* Toplevel implementation. To be plugged by the native or bytecode
   Topdirs implementation. *)

module Top = struct

  module type TOPDIRS = sig
    val dir_directory : string -> unit
    val dir_remove_directory : string -> unit
    val dir_use : Format.formatter -> string -> unit
    val dir_load : Format.formatter -> string -> unit
  end

  module Nil = struct
    let nil () = invalid_arg "Omod.Private.Top.set_topdirs not called"
    let dir_directory d = nil ()
    let dir_remove_directory d = nil ()
    let dir_use fmt file = nil ()
    let dir_load fmt file = nil ()
  end

  let topdirs : (module TOPDIRS) ref = ref (module Nil : TOPDIRS)
  let is_nat : bool ref = ref false
  let set_topdirs ~is_nat:b t = topdirs := t; is_nat := b

  let symtable_exn_to_string exn =
    (* Pattern match over Symtable.Error and Symtable.exception type
       to emulate Symtable.report_error. See bytecomp/symtable.ml in
       the compiler. *)
    let e = Obj.field (Obj.repr exn) 1 in
    let str e = (Obj.magic (Obj.field e 0) : string) in
    match Obj.tag e with
    | 0 (* Undefined_global *) ->
        strf "Reference to undefined global `%s'" (str e)
    | 1 (* Unavailable_primitive *) ->
        strf "The external function `%s' is not available" (str e)
    | 3 (* Wrong_vm *) ->
        strf "Cannot find or execute the runtime system %s" (str e)
    | 4 (* Uninitialized_global *) ->
        strf "The value of global `%s' is not yet computed" (str e)
    | n ->
        strf "Unknown Symtable.error case (%d) please report a bug to odig" n

  let exn_to_string bt e = match Printexc.exn_slot_name e with
  | "Symtable.Error" -> symtable_exn_to_string e
  | exn -> strf "Unknown exception:\n%s\n%s" exn bt

  let err_fmt, get_err =
    let buf = Buffer.create 255 in
    let fmt = Format.formatter_of_buffer buf in
    let get () =
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buf in
      Buffer.reset buf; s
    in
    fmt, get

  let handle_toploop_api f v =
    try
      let r = f err_fmt v in
      match get_err () with "" -> Ok r | err -> Error err
    with
    | e ->
        let bt = Printexc.get_backtrace () in
        Error (exn_to_string bt e)

  let handle_err d fpath r = match r with
  | Ok _ as v -> v
  | Error e -> Error (strf "%s %s:\n%s" d fpath e)

  let add_inc dir =
    let module Topdirs = (val !topdirs : TOPDIRS) in
    let add fmt dir = Topdirs.dir_directory dir in
    (handle_toploop_api add dir)
    |> handle_err "include" dir

  let rem_inc dir =
    let module Topdirs = (val !topdirs : TOPDIRS) in
    let rem fm dir = Topdirs.dir_remove_directory dir in
    (handle_toploop_api rem dir)
    |> handle_err "exclude" dir

  let load_ml ml =
    let module Topdirs = (val !topdirs : TOPDIRS) in
    (handle_toploop_api Topdirs.dir_use ml) |> handle_err "load" ml

  let load_obj obj =
    let module Topdirs = (val !topdirs : TOPDIRS) in
    (handle_toploop_api Topdirs.dir_load obj) |> handle_err "load" obj

  let is_nat () = !is_nat
end

(* Formatting *)

let pp_red = Fmt.tty_str ~mode:"31"
let pp_yellow = Fmt.tty_str ~mode:"33"
let pp_blue = Fmt.tty_str ~mode:"34"
let pp_magenta = Fmt.tty_str ~mode:"35"
let pp_code = Fmt.tty_str ~mode:"01" (* bold *)

(* Toplevel loaders *)

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf ("[%a] @[" ^^ fmt ^^ "@]@.") pp_red  "ERROR"
let log_error = function Ok v -> v | Error e -> log_err "%s" e

module Tinc = struct
  let included = ref String.Set.empty
  let is_included inc = String.Set.mem inc !included
  let add inc = match Top.add_inc inc with
  | Ok () -> Ok (included := String.Set.add inc !included)
  | Error _ as e -> e

  let rem inc =
    included := String.Set.remove inc !included;
    Top.rem_inc inc

  let reset () =
    String.Set.iter (fun inc -> log_error @@ rem inc) !included;
    included := String.Set.empty;
    ()

  let assumed = ref String.Set.empty
  let is_assumed obj = String.Set.mem obj !assumed
  let assume inc = assumed := String.Set.add inc !assumed

  let all () = String.Set.union !included !assumed

  let pp ppf s = Format.fprintf ppf "[%a] %s" pp_yellow "INC" s
end

module Tobj = struct
  let loaded = ref String.Set.empty
  let is_loaded obj = String.Set.mem obj !loaded

  let load obj = match Top.load_obj obj with
  | Ok () -> Ok (loaded := String.Set.add obj !loaded)
  | Error _ as e -> e

  let load_src src = match Top.load_ml src with
  | Ok () -> Ok (loaded := String.Set.add src !loaded)
  | Error _ as e -> e

  let reset () = loaded := String.Set.empty

  let assumed = ref String.Set.empty
  let is_assumed obj = String.Set.mem obj !assumed
  let assume obj = assumed := String.Set.add obj !assumed

  let all () = String.Set.union !loaded !assumed

  let pp ppf s = Fmt.pf ppf "[%a] %s" pp_blue "OBJ" s
  let pp_src ppf s = Fmt.pf ppf "[%a] %s" pp_magenta "SRC" s
end

(* Low-level loading *)

let add_inc ~assume ~silent ~force ~incs inc =
  if not incs then Ok () else
  if (Tinc.is_included inc || Tinc.is_assumed inc) && not force then Ok () else
  begin
    if not silent then log "%a" Tinc.pp inc;
    if assume then (Tinc.assume inc; Ok ()) else Tinc.add inc
  end

let load_init ~assume ~silent ~force ~init obj =
  if not init then Ok () else
  let init_base = Filename.(chop_extension @@ basename obj) ^ "_top_init.ml" in
  let init = Filename.(concat (dirname obj) init_base) in
  match Sys.file_exists init with
  | exception Sys_error e -> Error (strf "%s: %s" init e)
  | false -> Ok ()
  | true ->
      if (Tobj.is_loaded init || Tobj.is_assumed init) && not force then Ok ()
      else begin
        if not silent then log "%a" Tobj.pp_src init;
        if assume then (Tobj.assume init; Ok ()) else Tobj.load_src init
      end

let load_obj ~assume ~silent ~force obj =
  if (Tobj.is_loaded obj || Tobj.is_assumed obj) && not force then Ok () else
  begin
    if not silent then log "%a" Tobj.pp obj;
    if assume then (Tobj.assume obj; Ok ()) else Tobj.load obj
  end

let load_dance ~assume ~silent ~force ~incs ~init obj =
  match add_inc ~assume ~silent ~force ~incs (Filename.dirname obj) with
  | Error _ as e -> e
  | Ok () ->
      match Filename.check_suffix obj "cmi" with
      | true -> (* mli-only, [add_inc] made the include *) Ok ()
      | false ->
          match load_obj ~assume ~silent ~force obj with
          | Error _ as e -> e
          | Ok () ->
              match load_init ~assume ~silent ~force ~init obj with
              | Error _ as e -> log_error e (* non-fatal *); Ok ()
              | Ok () -> Ok ()

let load_objs ~assume ~silent ~force ~incs ~init objs =
  let rec loop = function
  | [] -> true
  | obj :: objs ->
      match load_dance ~assume ~silent ~force ~incs ~init obj with
      | Ok () -> loop objs
      | Error _ as e -> log_error e; false
  in
  loop objs

(* Load sequence requests *)

let omod_load_seqs ~silent mods =
  let rec parse_loads s =
    let rec loop acc cur = function
    | [] -> Ok (List.rev ((List.rev cur) :: acc))
    | "" :: ls -> loop acc cur ls
    | "error:" :: err_lines -> Error (String.concat "\n" err_lines)
    | "load:" :: ls ->
        if cur = [] then loop acc cur ls else
        loop (List.rev cur :: acc) [] ls
    | l :: ls -> loop acc (l :: cur) ls
    in
    loop [] [] (String.rev_cuts ~sep:'\n' s)
  in
  let add_if c arg acc = if c then arg :: acc else acc in
  let cmd =
    "omod" :: "load" :: "--force" ::
    (add_if (Top.is_nat ()) "--nat" @@ add_if silent "--quiet" @@ mods)
  in
  match Cmd.read cmd with Error (_, e) -> Error e | Ok v -> parse_loads v

let pp_load_sequences ppf seqs =
  let pp_seq pp i seq =
    Fmt.pf ppf "@[<v>[%d] loads:@,%a@]@," i Fmt.(list string) seq
  in
  List.iteri (pp_seq ppf) seqs

(* API *)

type silent = [ `Yes | `Loads | `No ]

let ask_which_seq ppf max =
  let parse s = match int_of_string (String.trim s) with
  | exception Failure _ -> Error (strf "%s: could not parse integer" s)
  | v ->
      if v < 0 || v > max then Error (strf "%d: out of [0;%d] range" v max)
      else Ok (Some v)
  in
  let k ppf = match input_line stdin with
  | exception End_of_file -> parse "" | s -> parse s
  | exception Sys.Break -> Ok None
  in
  Format.kfprintf k ppf
    "@[<1>Select a load sequence in 0-%d (C-c to abort): @]@?" max

let _load
    ~assume ?(batch = not !Sys.interactive) ?(silent = `No) ?(force = false)
    ?(incs = true) ?(init = true) ?dir mods
  =
  if mods = [] then true else
  let omod_silence = match silent with `Yes -> true | _ -> false in
  match omod_load_seqs ~silent:omod_silence mods with
  | Error _ as e -> log_error e; false
  | Ok loads ->
      let silent = match silent with `Yes | `Loads -> true | `No -> false in
      match loads with
      | [] -> assert false
      | [objs] -> load_objs ~assume ~silent ~force ~incs ~init objs
      | alts ->
          let log = if batch then log_err else log in
          log "@[<v>%a has alternative load sequences:@,@[<v>%a@]@]"
            Fmt.(list string) mods
            pp_load_sequences alts;
          if batch then false else
          match ask_which_seq Format.std_formatter (List.length alts - 1) with
          | Error _ as e -> log_error e; false
          | Ok None -> log ""; false
          | Ok (Some n) ->
              load_objs ~assume ~silent ~force ~incs ~init (List.nth alts n)


let loads = _load ~assume:false
let assume_loads = _load ~assume:true

let load ?batch ?silent ?force ?incs ?init ?dir m =
  _load ~assume:false ?batch ?silent ?force ?incs ?init ?dir [m]

let assume_load ?batch ?silent ?force ?incs ?init ?dir m =
  _load ~assume:true ?batch ?silent ?force ?incs ?init ?dir [m]

let assume_inc = Tinc.assume
let assume_obj = Tobj.assume

let pp_help ppf () =
  Fmt.pf ppf "@[<v>Commands:@,  %a load module %a@,\
              @[  %a list what is currently loaded by Omod@]"
    pp_code "Omod.load \"M\"" pp_code "M" pp_code "Omod.status ()"

let pp_status ppf () =
  let incs = String.Set.elements (Tinc.all ()) in
  let objs = String.Set.elements (Tobj.all ()) in
  Fmt.(pf ppf "@[<v>%a" (list Tinc.pp) incs);
  if incs <> [] && objs <> [] then Fmt.pf ppf "@,";
  Fmt.(pf ppf "%a@]" (list Tobj.pp) objs)

let pp_announce ppf () =
  Fmt.pf ppf "%a %%VERSION%% loaded. Type %a for more info.@."
    pp_yellow "Omod" pp_code "Omod.help ()"

let help () = Format.printf "%a@." pp_help ()
let status () = Format.printf "%a@." pp_status ()
let announce ?(ppf = Format.std_formatter) () = pp_announce ppf ()

module Private = struct
  let announce = announce
  module String = String
  module Fmt = Fmt
  module File = File
  module Cmd = Cmd
  module Top = Top
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
