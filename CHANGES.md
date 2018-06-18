
- Fix `omod` for opam system switches. The `ocaml` package
  directory is determined by `ocamlc -where` rather than
  `$LIBDIR/ocaml` (#1)
- Get rid of shell outs to `opam` for configuration detection:
  infer prefix from the executable path (#4).
- `opam pkg --long` sort cobjs by name.

v0.0.1 2018-06-13 Zagreb
------------------------

First release.
