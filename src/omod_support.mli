(*---------------------------------------------------------------------------
   Copyright (c) 2018 The omod programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [Omod] support library.

    This library is used to implement the [omod] tool. It should not
    be loaded in the toplevel; notably because it uses [compiler-lib]. *)

open Omod.Private

(** Value serializers.

    The underlying implementation uses {!Marshal} with magic numbers that
    depend on omod and OCaml's versions. *)
module Codec : sig
  type 'a t
  (** The type for codec of values of type ['a]. *)

  val v : id:string -> 'a t
  (** [v ~id] is a new codec with identifier [id] (part of the magic number). *)

  val write : 'a t -> Omod.fpath -> 'a -> (unit, string) result
  (** [write c file v] writes value [v] to [file] using codec [c]. *)

  val read : 'a t -> Omod.fpath -> ('a, string) result
  (** [read c file] reads a value from [file] using codec [c]. *)
end

(** Digests. *)
module Digest : sig
  include (module type of Digest)
  val pp : Format.formatter -> t -> unit
  (** [pp] formats digests. *)

  val pp_opt : Format.formatter -> t option -> unit
  (** [pp_opt] formats optional digests. *)

  (** Digest sets. *)
  module Set : Set.S with type elt = t

  (** Digest maps. *)
  module Map : Map.S with type key = t
end

(** Logging. *)
module Log : sig

  type t = { f : 'a. ('a, Format.formatter, unit) Pervasives.format -> 'a }
  (** The type for logging functions. *)

  val nil : t
  (** [nil] is a logging function that drops logging. *)

  val std : t
  (** [std] is a logging function that format on {!Format.std_formatter}
      and prepends messages by the executable basename. *)

  val err : t
  (** [err] is like {!std} but formats on {!Format.err_formatter}. *)

  val time : t -> string -> (unit -> 'a) -> 'a
  (** [time l label f] logs the processor time of [f ()] on [l]. *)
end

(** Compilation objects *)
module Cobj : sig

  (** {1:pkg_ids Package identifiers} *)

  type pkg_id = string * Omod.fpath
  (** The type for package identifiers. A package name and its
      root directory. *)

  val pp_pkg_id : Format.formatter -> pkg_id -> unit
  (** [pp_pkg_id] formats package identifiers. *)

  (** {1:deps Dependencies} *)

  type dep = string * Digest.t option
  (** The type for compilation object dependencies. A module name and
      an optional interface digest. *)

  val pp_dep : Format.formatter -> dep -> unit
  (** [pp_dep] formats a dependency. *)

  (** {1:specs Specifications} *)

  val spec_of_string : string -> string option * string * string list
  (** [spec_of_string s] parses a compilation object specification
      from [s]. This parses the [[PKG.]M(\@VARIANT)+] syntax. *)

  (** {1:cobjs Compilation objects} *)

  type kind = Cmi | Cmo | Cmx (** *)
  (** The type for kind of compilation objects. *)

  val kind_of_string : string -> kind option
  (** [kind_of_string s] parses [s] into a kind. *)

  val kind_to_string : kind -> string
  (** [kind_to_string k] is [k] as a string. *)

  val exts : Omod.Private.String.Set.t
  (** [exts] are the file extensions of compilation objects. *)

  type t
  (** The type for compilation objects. *)

  val v :
    kind:kind -> pkg_id:pkg_id -> name:string ->
    iface_digest:Digest.t option -> iface_deps:dep list ->
    in_archive:bool -> path:Omod.fpath -> path_loads:t list Lazy.t -> t
  (** [v] is a compilation object with the given parameters, see the
      documentation of accessors for semantics. *)

  val add_file :
    pkg_id:pkg_id -> t list -> Omod.fpath -> (t list, string) result
  (** [add_file ~pkg_id acc f] adds the compilation objects of file [f]
      in package [pkg_id] to [acc]. If [f]'s extension doesn't match
      a supported compilation object file this is [acc]. *)

  val kind : t -> kind
  (** [kind c] is [c]'s {{!type:kind}kind}. *)

  val pkg_id : t -> pkg_id
  (** [pkg_id c] is [c]'s package identifier. *)

  val name : t -> string
  (** [name c] is [c]'s capitalized module name (more precisely compilation
      unit name). *)

  val variant : t -> string
  (** [variant c] is [c]'s variant. *)

  val iface_digest : t -> Digest.t option
  (** [iface_digest c] is [c]'s interface digest (if any). *)

  val iface_deps : t -> dep list
  (** [iface_deps c] is [c]'s interface dependencies. *)

  val in_archive : t -> bool
  (** [in_archive c] is [true] if {!path} is an object archive. *)

  val path : t -> Omod.fpath
  (** [path c] is [c]'s file path. *)

  val path_loads : t -> t list
  (** [path_loads c] are all the objects that are loaded whenever
      {!path} is loaded (includes [c] itself). For paths that point to
      archives this has all the objects of the archive. *)

  val to_dep : t -> dep
  (** [to_dep c] is [c] as an interface dependency. *)

  (** {1:pred Predicates} *)

  val is_kind : kind -> t -> bool
  (** [is_kind k c] is [true] iff [kind c = k]. *)

  val equal : t -> t -> bool
  (** [equal c c'] tests [c] and [c'] for equality (all components
      must match). *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  val ui_compare : t -> t -> int
  (** [ui_compare] orders by name, package, variant and path. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a compilation object. *)

  (** {1 Indexes and dependency resolvers} *)

  (** Compilation object indexes. *)
  module Index : sig

    (** {1:index Indexes} *)

    type cobj = t
    (** See {!Cobj.t}. *)

    type t
    (** The type for compilation object indexes. *)

    val empty : t
    (** The type for empty indexes. *)

    val of_cobjs : ?init:t -> cobj list -> t
    (** [of_cobjs ~init cobjs] is an index made of [init] (defaults to
        {!empty}) and [cobjs]. *)

    val cobjs : t -> cobj list
    (** [to_cobjs i] are the compilation objects of [i]. *)

    val cobjs_by_name : t -> cobj list String.Map.t
    (** [cobjs_by_name i] are the module names of [i] and the compilation
        objects they map to. *)

    val cobjs_by_digest : t -> cobj list Digest.Map.t
    (** [cobjs_by_digest i] are the interface digests of [i] and the compilation
        objects they map to. *)

    val cobjs_by_pkg_name : t -> cobj list String.Map.t
    (** [cobjs_by_pkg_name i] are the package names of [i] and the
        compilation objects they map to. *)

    val cobjs_for_mod_name : string -> t -> cobj list
    (** [cobjs_for_mod_name n i] are the compilation objects of [i]
        whose {{!mod_name}module name} matches [n]. *)

    val cobjs_for_iface_digest : Digest.t -> t -> cobj list
    (** [cobjs_for_iface_digest d i] are the compilation objects of [i]
        whose {{!iface_digest}interface digest} matches [d]. *)

    val cobjs_for_pkg_name : string -> t -> cobj list
    (** [cobjs_for_pkg_name n i] are the compilation objects of [i]
        whose package name matches [n]. *)

    val cobjs_for_dep : dep -> t -> cobj list
    (** [cobjs_for_iface_digest dep i] are the compilation objects of [i]
        which satisfy dependency [dep]. *)

    val cobjs_for_dep_res :
      variants:String.Set.t -> sat:(cobj -> bool) -> kind:kind -> dep ->
      t -> cobj list
    (** [cobjs_for_dep_res ~variants ~sat ~kind dep i] resolves [dep]
        in [i] to a [sat] satisfying compilation object of kind [kind]
        with the following twists:
        {ul
        {- If no [kind] can be found but a [Cmi] exists the latter is
           returned, assuming an mli-only module.}
        {- If an object is available both as a standalone compilation
           object and in an archive, only resolutions that mention the
           archive are returned.}
        {- If an object is available in multiple variants and [variants]
           is not {!String.Set.empty} then variants that do not
           belong to [variants] are dropped.}} *)
  end

  type res = t String.Map.t
  (** The type for dependency resolutions. Maps module names to their
      resolved object. *)

  val resolve_deps :
    variants:String.Set.t -> sat:(t -> bool) -> kind:kind -> Index.t ->
    root_alts:t list list -> (res list, string) result
  (** [resolve_deps ~variants ~sat ~kind i roots] is a list of resolutions that
      recursively resolve the dependencies of the alternative root
      object roots [root_alts] to compilation objects
      of kind [kind] and satisfying [sat] using the twists of
      {!Index.cobjs_for_dep_res}. *)

  val fold_res : res -> (t -> 'a -> 'a) -> 'a -> 'a
  (** [fold_res res f acc] folds [f] with [acc] over the partial
      dependency order of [res] @raise Invalid_argument if the
      dependencies of objects in [res] are not defined in [res], this
      can't happen if the map is a result of {!resolve_deps}. *)

  val loads :
    variants:String.Set.t ->
    sat:(t -> bool) -> kind:kind -> Index.t -> root_alts:t list list ->
    (Omod.fpath list list, string) result
  (** [loads ~sat ~kind i root_alts] resolves the alternative root
      objects roots [root_alts] to alternative load
      sequences of object paths of that have objects of kind [kind] or
      [Cmi]s (mli-only modules). All the objects involved in the load
      sequence satisfy [sat]. *)
end

(** Packages.

    Packages represents sets of compilation objects indexed from
    a root {!Conf.libdir}. *)
module Pkg : sig

  (** {1:pkg Packages} *)

  type t = Cobj.pkg_id
  (** The type for packages. *)

  val of_dir : ?err:Log.t -> Omod.fpath -> t list
  (** [of_dir ~err dir] are the packages found in [dir]. This is
      simply all the directory names of [dir]. [err] is used to report
      file system errors (defaults to {!Log.err}). *)

  val find_cobjs :
    ?err:Log.t -> ?acc:Cobj.t list -> t -> Cobj.t list
  (** [find_cobjs ~err ~note ~acc pkg] are the compilation objects
      contained in pkg [pkg] added to [acc] (defaults to [[]]). [err]
      is used to report errors (defaults to {!Log.nil}). *)

  val equal : t -> t -> bool
  (** [equal p0 p1] is [true] iff [p0] and [p1] have the same identifiers. *)

  val compare : t -> t -> int
  (** [compare] is a total order on packages compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats package identifiers. *)

  val pp_name : Format.formatter -> t -> unit
  (** [pp_name] formats the name of package identifiers. *)

  (** Package identifier sets. *)
  module Set : Set.S with type elt = t

  (** Package identifier maps. *)
  module Map : Map.S with type key = t

  (** {1:signature Package signatures} *)

  type signature = Digest.t
  (** The type for package signatures. *)

  val signature : ?err:Log.t -> t -> signature
  (** [signature ~err pkg] is a signature for package [pkg]. This is
      the digest of the mtimes of all the cobjs of [pkg]. [err] is
      used to report error (defaults to {!Log.err}). *)

  (** {1:info Package information} *)

  type info
  (** The type for package information. *)

  val info : signature:signature -> cobjs:Cobj.t list -> info
  (** [info ~signature ~cobjs] is information for a package. *)

  val info_signature : info -> signature
  (** [info_signature i] is the package's signature. *)

  val info_cobjs : info -> Cobj.t list
  (** [info_signature i] is the package's compilation objects. *)

  val pp_info : Format.formatter -> info -> unit
  (** [pp_info] formats package information. *)

  (** {1:db Package databases} *)

  type db = info Map.t
  (** The type for package databases. Maps package names to their
      information. *)

  val db : ?err:Log.t -> ?note:Log.t -> ?init:db -> t list -> db
  (** [db ~err ~note ~init pkgs] is database [init] (dfeaults to
      {!Map.empty}) with packages [pkgs] added. Their information
      is computed by the function using {!err} to report errors
      (defaults to {!Log.err}) and [note] to report indexing
      operations (defaults to {!Log.nil}). *)

  val db_to_name_db : db -> (t * info) String.Map.t
  (** [db_to_name_db db] maps package names (rather than identifiers)
      to their information. *)

  val db_to_cobj_index : db -> Cobj.Index.t
  (** [db_to_cobj_index db] is a compilatino object with the
      contents of the packages of [db]. *)

  type diff =
  [ `New of t * signature
  | `Changed of t * signature
  | `Gone of t ]

  (** The type for package database differences.
      {ul
      {- [`New (pkg, sg)], package [pkg] with signature [sg] is new.}
      {- [`Gone pkg], package [pkg] was removed.}
      {- [`Changed (pkg, sg)], package [pkg] changed to signature [sg].}} *)

  val pp_diff : diff Fmt.t
  (** [pp_diff] formats package database differences. *)

  val diff : db -> (t * signature) list -> diff list
  (** [diff db sgs] is the list of difference between [db] and
      package signatures [sgs]. *)

  val update : ?err:Log.t -> ?note:Log.t -> db -> diff list -> db
  (** [udpate ~err ~note db diffs] is [db] updated according to [diffs].
      [err] is used to report errors (defaults to {!Log.err}) and
      [note] to report indexing operations (defaults to {!Log.nil}). *)
end

(** Omod configuration. *)
module Conf : sig

  type t
  (** The type for configuration. *)

  val v : ?libdir:Omod.fpath -> ?cache:Omod.fpath -> unit -> (t, string) result
  (** [v ~libdir ~cache ()] is a configuration with given [libdir] or
      and [cache]. If unspecified they are discovered. *)

  val libdir : t -> Omod.fpath
  (** [libdir c] is [c]'s library directory. *)

  val cache : t -> Omod.fpath
  (** [cache c] is [c]'s cache directory. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats configurations. *)

  (** {1:env Environment variables} *)

  val libdir_env : string
  (** [libdir_env] is the environment variable that can be used to
      define a libdir. *)

  val cache_env : string
  (** [cache_env] is the environment variable that can be used to
      define an cache directory. *)
end

(** Omod cache. *)
module Cache : sig

  type t
  (** The type for cached information. *)

  val v : pkgs:Pkg.db -> t
  (** [v ~pkgs] is a cache with package database [pkgs]. *)

  val pkgs : t -> Pkg.db
  (** [pkgs c] is [c]'s package database. *)

  val file : Conf.t -> Omod.fpath
  (** [file conf] is the cache file in configuration [conf]. *)

  val read : Conf.t -> force:bool -> err:Log.t -> (t option, string) result
  (** [read conf] is the cache of configuration [conf] (if any).
      If [force] is [true] and a cache read error occurs, it is logged
      on [err] and [Ok None] is returned. *)

  val write : Conf.t -> t -> (unit, string) result
  (** [write conf c] writes the cache [c] of configuration [conf]. *)

  val clear : Conf.t -> (unit, string) result
  (** [clear conf] clears the cache [c] of configuration [conf]. *)

  val get :
    ?err:Log.t -> ?note:Log.t -> Conf.t -> force:bool -> trust:bool ->
    (t, string) result
  (** [get ~err ~note conf ~force ~trust] get the cache of configuration
      [conf], forcing it if [force] is [true] and making sure it is
      fresh unless [trust] is [true], using [err] to report errors
      and [note] to report progress. *)

  val status : ?err:Log.t -> Conf.t -> t option -> Pkg.diff list
  (** [status ~err conf c] is the status of cache [c] of configuration
      [conf]. *)

  val codec : t Codec.t
  (** [codec] is a codec for caches. *)
end

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
