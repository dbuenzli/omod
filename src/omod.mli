(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Load installed modules in the toplevel.

    See the {{!page-tutorial}tutorial}. *)

(** {1 Omod} *)

type fpath = string
(** The type for file paths. *)

type silent = [ `Yes | `Loads | `No]
(** The type for specifying silence. See {!load}. *)

val load :
  ?batch:bool -> ?silent:silent -> ?force:bool ->
  ?incs:bool -> ?init:bool -> ?dir:fpath -> string -> bool
(** [load ~batch ~silent ~force ~deps ~incs ~init ~dir "M"] loads module [M]
    and returns [true] if the load was successful; init files may however
    have failed to load. The exact side effects of this function are
    described {{!loadsem}here}. The optional parameters are as follows:
    {ul
    {- [batch] if [true] alternative load sequences error rather than
       interactively ask to select one. Defaults to
       [not !]{!Sys.interactive}.}
    {- [silent] if [`All] nothing is logged except errors. If [`Loads]
       then load sequences are not logged but other diagnostic messages
       may be logged. If [`No] both load sequences and diagnostic messages
       are logged. Defaults to [`No].}
    {- [force] if [true] force the reload of objects. Defaults to [false].}
    {- [incs] if [true] directory includes should be added. Defaults
       to [true]. See load {{!loadsem}semantics} for details.}
    {- [init] if [true] toplevel init files should be loaded,
       see the load {{!loadsem}semantics}. Defaults to [true].}
    {- [dir] is currently ignored.}}

    The full syntax for specifying the module to load is:
{v [PKG.]M(@VARIANT)* v}
    {ul
    {- [M] is always the top level (compilation unit) module name to load.}
    {- [PKG] constrains [M] to be found in package [PKG]. Packages
       names are the name of directories just below omod's library
       directory (see [omod conf], and [omod pkg] for a list).}
    {- [@VARIANT] (repeatable) indicates that all ambiguities should be resolved
       according to variant [VARIANT]. This means that if an object
       can be found in multiple directories in a package directory
       [P], the one that is rooted in the hierarchy starting at [P/VARIANT]
       or [P/@VARIANT] will be selected. If no ambiguity arises the parameter
       is ignored. See the {{!tutorial}tutorial} for an example.}} *)

val loads :
  ?batch:bool -> ?silent:silent -> ?force:bool -> ?incs:bool -> ?init:bool ->
  ?dir:fpath -> string list -> bool
(** [loads] is like {!load} but for a list of module specifications.
    Note that specified variants apply to all of the modules. *)

val help : unit -> unit
(** [help ()] prints basic help on [stdout]. *)

val status : unit -> unit
(** [status ()] prints what is currently loaded by omod (including
    assumptions) *)

(** {1:assuming Assuming loads}

    This following can be used to assume that certain loads are
    already performed to prevent them from being (re)loaded by
    {!load} invocations. *)

val assume_load :
  ?batch:bool -> ?silent:silent -> ?force:bool -> ?incs:bool -> ?init:bool ->
  ?dir:fpath -> string -> bool
(** [assume_load] is like {!load} but assumes the corresponding load
    sequence was already performed. *)

val assume_loads :
  ?batch:bool -> ?silent:silent -> ?force:bool -> ?incs:bool -> ?init:bool ->
  ?dir:fpath -> string list -> bool
(** [assume_loads] is like {!loads} but assumes the corresponding load
    sequence was already performed. *)

val assume_inc : fpath -> unit
(** [assume_inc dir] assumes that path [dir] has been included. *)

val assume_obj : fpath -> unit
(** [assume_obj obj] assumes that file path [obj] has been loaded. *)

(** Private definitions. *)
module Private : sig

  val announce : ?ppf:Format.formatter ->  unit -> unit
  (** [announce] outputs a message indicating omod's toplevel support has
      been setup. *)

  (** Strings. *)
  module String : sig
    include (module type of String)

    val cut : sep:char -> string -> (string * string) option
    (** [cut ~sep s] cuts [s] on the left and right of the first char
        [sep] starting from the left. *)

    val rev_cut : sep:char -> string -> (string * string) option
    (** [rev_cut ~sep s] cuts [s] on the left and right of the first char
        [sep] starting from the right. *)

    val rev_cuts : sep:char -> string -> string list

    val starts_with : prefix:string -> string -> bool
    (** [starts_with ~prefix s] is [true] iff [prefix] is a prefix
        of [s]. {b Note.} Available in 4.13. *)

    val edit_distance : string -> string -> int
    (** [edit_distance s0 s1] is the number of single character edits
        (insertion, deletion, substitution) that are needed to change
        [s0] into [s1]. *)

    val suggest : ?dist:int -> string list -> string -> string list
    (** [suggest ~dist candidates s] are the elements of [candidates]
        whose {{!edit_distance}edit distance} is the smallest to [s] and
        at most at a distance of [dist] of [s] (defaults to [2]). If
        multiple results are returned the order of [candidates] is
        preserved. *)

    (** String sets *)
    module Set : Set.S with type elt = string

    (** String maps. *)
    module Map : Map.S with type key = string
  end

  (** Formatting. *)
  module Fmt : sig

    val ansi_tty : bool ref
    (** [ansi_tty] determines whether ANSI formatting is performed or not. *)

    type 'a t = Format.formatter -> 'a -> unit
    (** The type for formatters of value of type ['a]. *)

    val pf : Format.formatter ->
      ('a, Format.formatter, unit) Stdlib.format -> 'a
    (** [pf] is {!Format.fprintf}. *)

    val string : string t
    (** [string] is {!Format.pp_print_string}. *)

    val list : ?sep:unit t -> 'a t -> 'a list t
    (** [list] is {!Format.pp_print_list}. *)

    val tty_str : mode:string -> string t
    (** [tty_str ~mode] formats string with ANSI mode [mode]. *)

    val faint : string t
    (** [faint] formats a string with less contrast. *)
  end

  (** File IO. *)
  module File : sig

    val cut_ext : fpath -> string * string
    (** [cut_ext f] is [(fst, snd)] the pair that results from cutting
        the basename of [f] at the rightmost ['.'] (not included in the
        result) before the first {!Filename.dir_sep}. If there is no such
        character, fst is [f] and [snd] is [""]. *)

    val catch_sys_error : (unit -> ('a, string) result) -> ('a, string) result
    (** [catch_sys_error f] invokes [f ()] and catches any [Sys_error] that
        may be raised and returns its message [e] as [Error e]. *)

    val with_open_in : string -> (in_channel -> 'a) -> ('a, string) result
    (** [with_open_in file f] open a channel on [file] and gives it
        to [f]. Ensures the channel is closed when the function returns
        and reports any [Sys_error] or [End_of_file] as an [Error]. *)

    val with_open_out : string -> (out_channel -> 'a) -> ('a, string) result
    (** [with_open_out file f] open a channel on [file] and gives it
        to [f]. Ensures the channel is closed when the function returns
        and reports any [Sys_error] or [End_of_file] as an [Error]. *)

    val to_string : fpath -> (string, string) result
    (** [to_string file] reads file [file] to a string. *)

    val of_string : fpath -> string -> (unit, string) result
    (** [of_string file s] writes file [file] with [s]. *)

    val exists : fpath -> (bool, string) result
    (** [exists file] is [true] iff [file] exists and is not a directory. *)

    val delete : fpath -> (bool, string) result
    (** [delete file] deletes file [file]. The boolean indicates
        whether the file actually existed. *)
  end

  (** Executing commands. *)
  module Cmd : sig

    type t = string list
    (** The type for command line fragments *)

    val find : string list -> string option
    (** [find bins] looks for the first binary in [bins] in PATH. *)

    val run : t -> (unit, int * string) result
    (** [run cmd] runs the command [cmd]. In case of error returns
        the command's exit code and an error message. *)

    val read : t -> (string, int * string) result
    (** [run cmd] runs the command [cmd] and reads its standard input.
        In case of error returns the command's exit code and an error
        message. *)
  end

  (** Abstracts away the OCaml Toploop API. *)
  module Top : sig

    (** {1:topdir Toplevel directives} *)

    val add_inc : string -> (unit, string) result
    (** [add_inc dir] adds [dir] to the include path. *)

    val rem_inc : string -> (unit, string) result
    (** [rem_inc dir] removes [dir] from the include path. *)

    val load_ml : string -> (unit, string) result
    (** [load_ml ml] loads the source file [ml]. *)

    val load_obj : string -> (unit, string) result
    (** [load_obj obj] loads the object file [obj]. *)

    val is_nat : unit -> bool
    (** [is_nat ()] is [true] if the current toplevel implementation
        is [ocamlnat]. *)

    (** {1:topdirimpl Topdirs implementations} *)

    (** The type for Topdir module implementations. *)
    module type TOPDIRS = sig
      val dir_directory : string -> unit
      val dir_remove_directory : string -> unit
      val dir_use : Format.formatter -> string -> unit
      val dir_load : Format.formatter -> string -> unit
    end

    val set_topdirs : is_nat:bool -> (module TOPDIRS) -> unit
    (** [set_topdirs ~is_nat t] sets the topdirs implementation to [t]
        and indicates with [is_nat] if this [ocamlnat]'s implementation. *)
  end
end

(**  {1:loadsem Load semantics and effects}

    {ul

    {- Loading an object means: load its dependencies,
       add its containing directory to the
       included directories (if [incs] is [true]), load the actual object
       and finally load its toplevel init file (if [init] is [true] and the
       file exists, see below).}
    {- The {e toplevel init file} of an object with basename [o] is a file
       called [o_top_init.ml] in the same directory as the object.}
    {- If an object is available both as a standalone file ([cmo], [cmx])
       and in a library archive ([cma], [cmxs]), [Omod] favours loading the
       library archive.}
    {- If an interface dependency cannot be resolved to an implementation
       but does resolve to a compiled interface, the dependency
       is assumed to be a mli-only compilation unit and the directory
       of the compiled interface is added to the includes (if [incs] is true).}
    {- The initialization performed by [omod.top] and [omod.nattop]
       {{!assume_load}assume} (with [incs:false] and [init:false])
       the following modules:
       {ul
       {- [utop.UTop] if [omod.top] is [#use]d in [utop]. }
       {- [ocaml.Toploop] if [omod.top] is [#use]d (not in [utop]).}
       {- [ocaml.Opttoploop] if [omod.nattop] is [#use]d. .}}}
    {- Load sequences with [vmthread] variants and objects
       of the form [m.p.ext] (profiling versions in the stdlib)
       are excluded from load sequence results. This reduces the load
       sequence from multiple occurrences to a single candidate on many
       modules.}
    {- For [ocamlnat] dependency analysis is made on [cmx] and [cmxa] files,
       the suffixes of resulting objects is then mapped to [cmxs]. This
       assumes the corresponding files exist and their objects
       match.}} *)

(*
    {2:localsearch Local search}

    Some functions take a [~dir] argument that specifies a directory
    where objects can be looked up in addition to packages.  This
    directory defaults to [_build] or the value of the environment
    value [ODIG_TOP_LOCAL_DIR]. These load functions always first look up
    for objects locally and then in packages.
*)


(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers

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
