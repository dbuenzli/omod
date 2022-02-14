
- Allow to abort load sequence prompts cleanly with C-c (#9).
- Fix variant specification in `Omod.load` (#11).
- Handle the deprecation of `Pervasives` (and thus support OCaml 5.00).
- `omod pkg`, order package info as found on the cli.

v0.0.2 2018-06-19 Zagreb
------------------------

- Fix `omod` for `opam` system switches. The location of the
  `ocaml` package is determined by `ocamlc -where` rather than
  `$LIBDIR/ocaml` (#1)
- Get rid of shell outs to `opam` for auto-configuration: infer prefix from the
  executable's install path (#4).
- `omod pkg --long` sort cobjs by name.

v0.0.1 2018-06-13 Zagreb
------------------------

First release.
