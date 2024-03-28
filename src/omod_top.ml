(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let init () =
  Omod.Private.Top.set_topdirs ~is_nat:false (module Topdirs);
  Omod.Private.announce ();
  let is_utop = Option.is_some (Toploop.get_directive "utop_help") in
  let base = if is_utop then "utop.UTop" else "ocaml.Toploop" in
  ignore @@
  Omod.assume_load ~batch:true ~silent:`Loads ~incs:false ~init:false base;;

let () = if !Sys.interactive then init () else ()
