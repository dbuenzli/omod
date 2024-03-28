omod — Lookup and load installed OCaml modules
==============================================

Omod is a library and command line tool to lookup and load installed
OCaml modules. It provides a mechanism to load modules and their
dependencies in the OCaml toplevel system (REPL).

omod is distributed under the ISC license.

Homepage: http://erratique.ch/software/omod  

# Installation

omod can be installed with `opam`:

    opam install omod

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Usage

    rlwrap ocaml
    # #use "omod.top"
    # Omod.load "Unix"
    # Omod.status ()

# Documentation

The documentation can be consulted [online] or via `odig doc omod`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker. 

[online]: https://erratique.ch/software/omod/doc
[OCaml forum]: https://discuss.ocaml.org/

