

- Stop using Toploop.directive_table, this means 
  OCaml 4.13 is required.

v0.0.3 2022-02-14 La Forclaz (VS)
---------------------------------

- Allow to abort load sequence prompts cleanly with C-c (#9).
- Fix variant specification in `Omod.load` (#11).
- Move init sequences from `omod.[nat]top` to the loaded `omod.cma`
  and `omod_nattop.cmxs`. As a side effect removes
  the annoying warning on load visible since 4.13 (#13) and allows
  to load `omod` via `ocamlafind` (i.e. via `#require`, not recommended
  but works).
- Require OCaml 4.08.
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
