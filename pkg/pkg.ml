#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

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
    let lib_dir = Conf.value c lib_dir in
    let subst_lib_dir file =
      OS.File.read file >>= fun contents ->
      OS.File.write_subst file ["LIBDIR", lib_dir] contents
    in
    subst_lib_dir "src/omod.top" >>= fun () ->
    subst_lib_dir "src/omod.nattop"

let pre c = top_config c
let build = Pkg.build ~pre ()

let () =
  Pkg.describe "omod" ~build @@ fun c ->
  Ok [ Pkg.mllib ~api:["Omod"] "src/omod.mllib";
       Pkg.mllib "src/omod_support.mllib";
       Pkg.toplevel "src/omod.top";
       Pkg.toplevel "src/omod.nattop";
       Pkg.bin "src/omod_bin" ~dst:"omod";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
       Pkg.test "test/test"; ]
