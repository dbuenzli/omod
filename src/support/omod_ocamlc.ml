(*---------------------------------------------------------------------------
   Copyright (c) 2016 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Error handling *)

let strf = Format.asprintf
let failwithf fmt = Format.kasprintf (fun s -> failwith s) fmt
let failwith_magic_mismatch ~kind ~found ~expected =
  failwithf "not a %s file, found magic %S, expected %S"
    kind found expected

let pp_cmt_format_error ppf = function
| Cmt_format.Not_a_typedtree s -> Format.fprintf ppf  "not a typed tree: %s" s

let error_msgf fmt = Format.kasprintf (fun s -> Error s) fmt
let exn_to_error f v = try f v with
| Sys_error e | Failure e -> error_msgf "%s" e
| End_of_file -> error_msgf "Unexpected end of file"
| Cmi_format.Error e -> error_msgf "%a" Cmi_format.report_error e
| Cmt_format.Error e -> error_msgf "%a" pp_cmt_format_error e

let prefix_path_on_error file r = match r with
| Ok _ as v -> v
| Error e -> error_msgf "%s: %s" file e

let with_ic fpath f v =
  exn_to_error begin fun () ->
    let ic = open_in fpath in
    try let r = f ic v in close_in ic; Ok r
    with e -> (try ignore (close_in ic) with e -> ()); raise e
  end ()

(* Misc tools *)

let name_of_fpath fpath =
  let basename = Filename.basename fpath in
  match Omod.Private.String.rev_cut ~sep:'.' basename with
  | None -> basename
  | Some (name, _ext) -> name

let split_dep name deps =
  (* splits digest of [name] from [deps], errors if [name] not in [deps].
     Raises Failure *)
  let rec loop digest acc = function
  | [] ->
      begin match digest with
      | None -> failwithf "self-digest for %s not found" name
      | Some digest -> (digest, List.rev acc)
      end
  | (n, digest') :: deps when n = name ->
      begin match digest with
      | None -> loop digest' acc deps
      | Some d ->
          begin match digest' with
          | None ->
              loop digest acc deps
          | Some d' when d = d' ->
              (* Cf. https://github.com/ocaml/ocaml/pull/744 *)
              loop digest acc deps
          | Some d' ->
              failwithf "multiple self-digest for %s (%s and %s)"
                name (Digest.to_hex d) (Digest.to_hex d')
          end
      end
  | (n, _ as dep) :: deps -> loop digest (dep :: acc) deps
  in
  loop None [] deps

let read_magic ~kind ~magic ic =
  let len = String.length magic in
  let found = really_input_string ic len in
  if not (String.equal found magic)
  then failwith_magic_mismatch ~kind ~found ~expected:magic;
  ()

let seek_data ~kind ~magic ic =
  read_magic ~kind ~magic ic;
  seek_in ic (input_binary_int ic);
  ()

(* Objects with dependency information *)

module type DOBJ = sig
  type t
  val read : Omod.fpath -> (t, string) result
  val name : t -> string
  val iface_digest : t -> Digest.t
  val iface_deps : t -> (string * Digest.t option) list
end

type dobj =
  { name : string;
    iface_digest : Digest.t;
    iface_deps : (string * Digest.t option) list; }

module Dobj = struct
  let name cmi = cmi.name
  let iface_digest cmi = cmi.iface_digest
  let iface_deps cmi = cmi.iface_deps
end

(* Cmi files *)

module Cmi = struct
  type t = dobj

  let read cmi =
    exn_to_error begin fun () ->
      let info = Cmi_format.read_cmi cmi in
    let name = info.Cmi_format.cmi_name in
      let iface_digest, iface_deps = split_dep name info.Cmi_format.cmi_crcs in
      Ok { name; iface_digest; iface_deps; }
    end ()
    |> prefix_path_on_error cmi

  include Dobj
end

(* Cmti files *)

module Cmti = struct
  type t = dobj

  let read cmti =
    exn_to_error begin fun () ->
      let info = Cmt_format.read_cmi cmti in
      let name = info.Cmi_format.cmi_name in
      let iface_digest, iface_deps = split_dep name info.Cmi_format.cmi_crcs in
      Ok { name; iface_digest; iface_deps; }
    end ()
    |> prefix_path_on_error cmti

  include Dobj
end

(* Cmo files. *)

module Cmo = struct
  type t = dobj

  let of_compilation_unit cu =
    let name = cu.Cmo_format.cu_name in
    let iface_digest, iface_deps = split_dep name cu.Cmo_format.cu_imports in
    { name; iface_digest; iface_deps }

  let of_in_channel ic () =
    seek_data ~kind:"cmo" ~magic:Config.cmo_magic_number ic;
    of_compilation_unit (input_value ic : Cmo_format.compilation_unit)

  let read cmo =
    exn_to_error (with_ic cmo of_in_channel) ()
    |> prefix_path_on_error cmo

  include Dobj
end

(* Cmt files *)

module Cmt = struct
  type t = dobj

  let read cmt =
    exn_to_error begin fun () ->
      let info = Cmt_format.read_cmt cmt in
      let name = info.Cmt_format.cmt_modname in
      let iface_digest, iface_deps =
        split_dep name info.Cmt_format.cmt_imports
      in
      Ok { name; iface_digest; iface_deps }
    end ()
  |> prefix_path_on_error cmt

  include Dobj
end

(* Cma files *)

module Cma = struct
  type t =
    { name : string;
      cmos : Cmo.t list;
      custom : bool;
      custom_cobjs : string list;
      custom_copts : string list;
      dllibs : string list; }

  let of_library fpath l =
    let name = name_of_fpath fpath in
    let cmos = List.map Cmo.of_compilation_unit l.Cmo_format.lib_units in
    let custom = l.Cmo_format.lib_custom in
    let custom_cobjs = l.Cmo_format.lib_ccobjs in
    let custom_copts = l.Cmo_format.lib_ccopts in
    let dllibs = l.Cmo_format.lib_dllibs in
    { name; cmos; custom; custom_cobjs; custom_copts; dllibs }

  let cma_of_in_channel ic fpath =
    seek_data ~kind:"cma" ~magic:Config.cma_magic_number ic;
    of_library fpath (input_value ic : Cmo_format.library)

  let read cma =
    exn_to_error (with_ic cma cma_of_in_channel) cma
    |> prefix_path_on_error cma

  let name cma = cma.name
  let cmos cma = cma.cmos
  let custom cma = cma.custom
  let custom_cobjs cma = cma.custom_cobjs
  let custom_copts cma = cma.custom_copts
  let dllibs cma = cma.dllibs
end

(* Cmx files. *)

module Cmx = struct
  type t =
    { name : string;
      digest : Digest.t;
      iface_digest : Digest.t;
      iface_deps : (string * Digest.t option) list;
      cmx_deps : (string * Digest.t option) list; }

  let of_compilation_unit (cu, digest) =
    let name = cu.Cmx_format.ui_name in
    let iface_digest, iface_deps =
      split_dep name cu.Cmx_format.ui_imports_cmi
    in
    let cmx_deps = cu.Cmx_format.ui_imports_cmx in
    { name; digest; iface_digest; iface_deps; cmx_deps }

  let of_in_channel ic () =
    read_magic ~kind:"cmx" ~magic:Config.cmx_magic_number ic;
    let cu = (input_value ic : Cmx_format.unit_infos) in
    let digest = Digest.input ic in
    of_compilation_unit (cu, digest)

  let read cmx =
    exn_to_error (with_ic cmx of_in_channel) ()
    |> prefix_path_on_error cmx

  let name cmx = cmx.name
  let digest cmx = cmx.digest
  let iface_digest cmx = cmx.iface_digest
  let iface_deps cmx = cmx.iface_deps
  let cmx_deps cmx = cmx.cmx_deps
end

(* Cmxa files *)

module Cmxa = struct
  type t =
    { name : string;
      cmxs : Cmx.t list;
      cobjs : string list;
      copts : string list; }

  let of_library fpath l =
    let name = name_of_fpath fpath in
    let cmxs = List.map Cmx.of_compilation_unit l.Cmx_format.lib_units in
    let cobjs = l.Cmx_format.lib_ccobjs in
    let copts = l.Cmx_format.lib_ccopts in
    { name; cmxs; cobjs; copts; }

  let of_in_channel ic fpath =
    read_magic ~kind:"cmxa" ~magic:Config.cmxa_magic_number ic;
    of_library fpath (input_value ic : Cmx_format.library_infos)

  let read cmxa =
    exn_to_error (with_ic cmxa of_in_channel) cmxa
    |> prefix_path_on_error cmxa

  let name cmxa = cmxa.name
  let cmxs cmxa = cmxa.cmxs
  let cobjs cmxa = cmxa.cobjs
  let copts cmxa = cmxa.copts
end

(* Cmxs files *)

module Cmxs = struct
  type t = { name : string; }

  let of_library fpath () =
    let name = name_of_fpath fpath in
    { name }

  let read cmxa =
    let exists = try Sys.file_exists cmxa with
    | Sys_error e -> false
    in
    if exists then Ok (of_library cmxa ()) else
    error_msgf "%s: No such file." cmxa

  let name cmxs = cmxs.name
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The omod programmers

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
