(*---------------------------------------------------------------------------
   Copyright (c) 2016 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml compilation artefacts readers. *)

(** The type for compilation objects with interface dependency information. *)
module type DOBJ = sig
  type t
  (** The type for the object files. *)

  val read : Omod.fpath -> (t, string) result
  (** [read f] reads an object file from [f]. *)

  val name : t -> string
  (** [name o] is the object name of [o]. *)

  val iface_digest : t -> Digest.t
  (** [iface_digest o] is the interface digest of [o]. *)

  val iface_deps : t -> (string * Digest.t option) list
  (** [iface_deps o] are the interfaces [o] depends on; without itself. *)
end

(** [cmi] files *)
module Cmi : sig
  include DOBJ
end

(** [cmti] files *)
module Cmti : sig
  include DOBJ
end

(** [cmo] files *)
module Cmo : sig
  include DOBJ
end

(** [cmt] files *)
module Cmt : sig
  include DOBJ
end

(** [cma] files *)
module Cma : sig

  type t
  (** The type for cma files. *)

  val read : Omod.fpath -> (t, string) result
  (** [read_cma f] reads a cma file from [f]. *)

  val name : t -> string
  (** [name cma] is the archive name of [cma]. *)

  val cmos : t -> Cmo.t list
  (** [cmos cma] are the [cmo]s contained in the archive. *)

  val custom : t -> bool
  (** [custom cma] is [true] if it requires custom mode linking. *)

  val custom_cobjs : t -> string list
  (** [custom_cobjs] are C objects files needed for custom mode linking. *)

  val custom_copts : t -> string list
  (** [custom_copts] are extra options passed for custom mode linking. *)

  val dllibs : t -> string list
  (** [dllibs] are dynamically loaded C libraries for ocamlrun dynamic
      linking. *)
end

(** [cmx] files *)
module Cmx : sig
  include DOBJ

  val digest : t -> Digest.t
  (** [digest cmx] is the implementation digest of [cmx]. *)

  val cmx_deps : t -> (string * Digest.t option) list
  (** [cmx_cmx_deps cmx] are the implementations [cmx] depends on. *)
end

(** [cmxa] files *)
module Cmxa : sig

  type t
  (** The type for [cmxa] files *)

  val read : Omod.fpath -> (t, string) result
  (** [read_cmxa f] reads a cmxa file from [f]. *)

  val name : t -> string
  (** [name cmxa] is the archive name of [cmxa]. *)

  val cmxs : t -> Cmx.t list
  (** [cmxs cmxa] are the [cmx]s contained in the archive. *)

  val cobjs : t -> string list
  (** [cobjs cmxa] are C objects files needed for linking. *)

  val copts : t -> string list
  (** [copts cmxa] are options for the C linker. *)
end

(** [cmxs] files *)
module Cmxs : sig
  type t
  (** The type for cmxs files *)

  val read : Omod.fpath -> (t, string) result
  (** [read f] reads a cmxs files from [f].

    {b Warning.} Simply checks the file exists. *)

  val name : t -> string
  (** [name cmxs] is the archive name of [cmxx]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The omod programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
