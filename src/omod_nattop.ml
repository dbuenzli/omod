(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let init () =
  Omod.Private.Top.set_topdirs ~is_nat:true (module Topdirs);
  Omod.Private.announce ();
  let base = "ocaml.Toploop" in
  ignore @@
  Omod.assume_load ~batch:true ~silent:`Loads ~init:false ~incs:false base

let () = if !Sys.interactive then init () else ()
