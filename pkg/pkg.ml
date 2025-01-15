#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let copy src dst =
  Log.info (fun m -> m "cp %S %S" src dst);
  OS.File.read src >>= fun content ->
  let content = strf "# 1 %S\n%s" src content in
  OS.File.write dst content

let ocaml_conditional c =
  let maj, min, _, _ = Conf.OCaml.version (Conf.OCaml.v c `Host_os) in
  let dst = "src/support/omod_cu.ml" in
  match (maj, min) < (5,2) with
  | true  -> copy "src/support/omod_cu_pre_520/omod_cu.ml" dst
  | false -> copy "src/support/omod_cu_geq_520/omod_cu.ml" dst

let lib_dir =
  let doc = "Use $(docv) as the lib directory" in
  let absent () =
    let opam = Conf.tool "opam" `Host_os in
    OS.Cmd.(run_out Cmd.(opam % "var" % "lib") |> to_string)
  in
  Conf.(discovered_key "lib-dir" fpath ~absent ~doc)

let top_config c = match Conf.build_context c with
| `Dev -> Ok ()
| `Pin | `Distrib ->
    let lib_dir = String.escaped (Conf.value c lib_dir) in
    let subst_lib_dir file =
      OS.File.read file >>= fun contents ->
      OS.File.write_subst file ["LIBDIR", lib_dir] contents
    in
    subst_lib_dir "src/omod.top"

let pre c =
  top_config c >>= fun () ->
  ocaml_conditional c

let build = Pkg.build ~pre ()

let () =
  Pkg.describe "omod" ~build @@ fun c ->
  Ok [ Pkg.mllib ~api:["Omod"] "src/omod.mllib";
       Pkg.mllib ~dst_dir:"support" "src/support/omod_support.mllib";
       Pkg.toplevel "src/omod.top";
       Pkg.bin "src/omod_bin" ~dst:"omod";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld" ]
