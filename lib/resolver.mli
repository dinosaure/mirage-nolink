open Rresult

module Artifact : sig
  type kind = CMI | CMO | CMX
  type dep = string * Digest.t option
  type t

  val pp : t Fmt.t
  val kind : t -> kind 
  val name : t -> string
  val path : t -> Fpath.t
  val iface_digest : t -> Digest.t option
  val iface_deps : t -> dep list
  val in_archive : t -> bool
  val cobjs : t -> string list
  val copts : t -> string list

  val of_path : Fpath.t -> (t list, [> Caml_objinfo.error ]) result
  val to_dep : t -> dep
end

module type GAMMA = sig
  type t
  type 'a select

  val of_name :
    string ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list

  val of_digest : Digest.t -> t -> Artifact.t list

  val of_dep :
    Artifact.dep ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list

  val cardinal: t -> int
  val populate : Artifact.t list -> t -> t
  val empty : t

  val add_object :
    name:string ->
    Fpath.t ->
    t ->
    (t, [> `Invalid_object of Fpath.t | `Msg of string | `Not_found of Fpath.t ]) result

  val find :
    select:('a ->
            Artifact.dep ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list select) ->
    'a ->
    Artifact.dep ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list
end

module Gamma (Selective : sig include Selective.S val run : 'a t -> 'a end) : GAMMA with type 'a select = 'a Selective.t

type t
type deps

module Make (Gamma : GAMMA) : sig
  type 'key error =
    [ `Msg of string
    | `Unresolved_dependency of ('key * Artifact.dep) ]

  val resolve :
    (Artifact.t -> 'a) ->
    select:('a ->
            Artifact.dep ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list Gamma.select) ->
    srcs:[ `Caml of Artifact.t | `Object of Fpath.t ] list list ->
    Gamma.t ->
    (t list, [> 'a error ]) result
end
