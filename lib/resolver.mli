open Rresult

module Caml_name : sig
  type t = private string

  val v : string -> t
  val to_string : t -> string
  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Caml_obj : sig
  type kind = CMI | CMO | CMX
  type dep = Caml_name.t * Digest.t option
  type t

  val pp : t Fmt.t
  val kind : t -> kind
  val name : t -> Caml_name.t
  val path : t -> Fpath.t
  val iface_digest : t -> Digest.t option
  val iface_deps : t -> dep list
  val in_archive : t -> bool
  val cobjs : t -> string list
  val copts : t -> string list
  val link : bool -> t -> t

  val of_path : Fpath.t -> (t list, [> Caml_objinfo.error ]) result
  val to_dep : t -> dep
end

type error =
  [ `Not_found of Fpath.t
  | `Invalid_object of Fpath.t
  | `Msg of string ]

val pp_error : error Fmt.t

module Obj : sig
  type t

  val of_path : name:Caml_name.t -> Fpath.t -> (t, [> error ]) result
  val name : t -> Caml_name.t
  val path : t -> Fpath.t
  val pp : t Fmt.t
end

type artifact = [ `Caml of Caml_obj.t | `Object of Obj.t ]

module type GAMMA = sig
  type t
  type 'a select

  val of_name : Caml_name.t -> t -> artifact list
  val of_digest : Digest.t -> t -> Caml_obj.t list
  val of_dep : Caml_obj.dep -> t -> artifact list

  val cardinal: t -> int
  val populate : Caml_obj.t list -> t -> t
  val add_object : name:Caml_name.t -> Fpath.t -> t -> (t, error) result
  val empty : t

  val find : select:('a -> Caml_obj.dep -> (artifact list -> artifact list) select) -> 'a -> Caml_obj.dep -> t -> artifact list
end

module Gamma (Selective : sig include Selective.S val run : 'a t -> 'a end) : GAMMA with type 'a select = 'a Selective.t

type t
type deps

module Make (Gamma : GAMMA) : sig
  type 'key error =
    [ `Msg of string
    | `Unresolved_dependency of ('key * Caml_obj.dep) ]

  val resolve :
    (Caml_obj.t -> 'a) ->
    select:('a -> Caml_obj.dep -> (artifact list -> artifact list) Gamma.select) ->
    srcs:artifact list list ->
    Gamma.t ->
    (t list, [> 'a error ]) result

  val iter : f:(Caml_name.t -> artifact -> unit) -> t -> unit
end
