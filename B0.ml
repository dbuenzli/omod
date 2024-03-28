open B0_kit.V000
open Result.Syntax

(* OCaml libraries *)

let omod = B0_ocaml.libname "omod"
let omod_nattop = B0_ocaml.libname "omod.nattop"
let omod_support = B0_ocaml.libname "omod.support"

let cmdliner = B0_ocaml.libname "cmdliner"
let compiler_libs_common = B0_ocaml.libname "compiler-libs.common"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"
let unix = B0_ocaml.libname "unix"

(* Omod libraries *)

let omod_srcs = [`File ~/"src/omod.mli"; `File ~/"src/omod.ml" ]
let omod_lib =
  let doc = "The omod library" in
  let srcs = `File ~/"src/omod_top.ml" :: omod_srcs in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib ~name:"omod-lib" omod ~requires ~srcs ~doc

let omod_lib_nat = (* Not added to the default pack for now *)
  let doc = "The omod library for ocamlnat" in
  let srcs = `File ~/"src/omod_nattop.ml" :: omod_srcs in
  let requires = [compiler_libs_toplevel] in
  B0_ocaml.lib ~name:"omod-nat-lib" omod_nattop ~requires ~srcs ~doc

let omod_support_lib =
  let doc = "The omod.support library" in
  let srcs = [`Dir ~/"src/support"]; in
  let requires = [omod; unix; compiler_libs_common] in
  B0_ocaml.lib omod_support ~srcs ~doc ~requires

(* Omod tool *)

let omod_tool =
  let doc = "The omod support tool" in
  let srcs = Fpath.[`File (v "src/omod_bin.ml")] in
  let requires = [cmdliner; omod; omod_support] in
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
      [ "ocaml", {|>= "4.13.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "cmdliner", {|>= "1.1.0"|}; ]
    |> ~~ B0_opam.install {|
      # Following is only to deal with
      # https://caml.inria.fr/mantis/view.php?id=7808
      [["install" "-d" "%{lib}%/ocaml/"]
       ["install" "src/omod.top" "src/omod.nattop" "%{lib}%/ocaml/"]]|}
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"omod package" ~meta ~locked:true @@
  [omod_lib; omod_support_lib; omod_tool]
