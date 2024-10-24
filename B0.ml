open B0_kit.V000
open Result.Syntax

(* OCaml libraries *)

let omod = B0_ocaml.libname "omod"
let omod_support = B0_ocaml.libname "omod.support"

let cmdliner = B0_ocaml.libname "cmdliner"
let compiler_libs_common = B0_ocaml.libname "compiler-libs.common"
(* let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel" *)

let unix = B0_ocaml.libname "unix"

(* Omod libraries *)

let omod_srcs = [`File ~/"src/omod.mli"; `File ~/"src/omod.ml" ]
let omod_lib =
  let doc = "The omod library" in
  let srcs = `File ~/"src/omod_top.ml" :: omod_srcs in
  let requires = [compiler_libs_common] in
  B0_ocaml.lib ~name:"omod-lib" omod ~requires ~srcs ~doc

let ocaml_cond b =
  (* TODO for this to work we need a corresponding mli (whatever
     the content) in the directories with the
     implementation. See https://github.com/ocaml/ocaml/issues/9717 *)
  let open Fut.Syntax in
  let* version = B0_ocaml.Conf.version' b in
  let dir = match version with
  | v when v < (5, 2, 0, None) -> "omod_cu_pre_520"
  | _ -> "omod_cu_geq_520"
  in
  let scope_dir = B0_build.scope_dir b in
    let file = Fpath.(scope_dir / "src" / "support" / dir / "omod_cu.ml") in
  B0_memo.ready_file (B0_build.memo b) file;
  Fut.return (Fpath.Set.singleton file)

let omod_support_lib =
  let doc = "The omod.support library" in
  let srcs = [
    `Dir ~/"src/support";
    `Fut ocaml_cond;
    `X ~/"src/support/omod_cu.ml"; (* Remove when we rid of topkg *)];
  in
  let requires = [omod; unix; compiler_libs_common;] in
  B0_ocaml.lib omod_support ~srcs ~doc ~requires

(* Omod tool *)

let omod_tool =
  let doc = "The omod support tool" in
  let srcs = [`File ~/"src/omod_bin.ml";]
  in
  let requires = [cmdliner; unix;
                  compiler_libs_common;
                  omod; omod_support]
  in
  B0_ocaml.exe "omod" ~public:true ~srcs ~doc ~requires

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The omod programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/omod"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/omod/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/omod.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/omod/issues"
    |> ~~ B0_meta.description_tags ["dev"; "toplevel"; "repl"; "org:erratique"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build"
         "--dev-pkg" "%{dev}%"
         "--lib-dir" "%{lib}%"]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "cmdliner", {|>= "1.1.0"|}; ]
    |> ~~ B0_opam.install {|
      # Following is only to deal with
      # https://caml.inria.fr/mantis/view.php?id=7808
      [["install" "-d" "%{lib}%/ocaml/"]
       ["install" "src/omod.top" "%{lib}%/ocaml/"]]|}
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"omod package" ~meta ~locked:true @@
  [omod_lib; omod_support_lib; omod_tool]
