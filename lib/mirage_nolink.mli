open Resolver

type ident =
  { name : Caml_name.t
  ; kind : Caml_obj.kind
  ; path : Fpath.t }

type resolver =
  ident -> Digest.t option -> artifact list -> artifact list * artifact list

val to_ident : Caml_obj.t -> ident
val register : name:Caml_name.t -> resolver -> unit
val select : kind:Caml_obj.kind -> ident -> Caml_obj.dep -> artifact list -> artifact list
val take : kind:Caml_obj.kind -> ?archive:bool -> name:Caml_name.t -> artifact list -> artifact list

module Gamma : GAMMA with type 'a select = 'a
module Resolver : module type of Make(Gamma)

type error = ident Resolver.error

val pp_error : error Fmt.t
val resolve : kind:Caml_obj.kind -> srcs:artifact list list -> Gamma.t -> (t list, [> error ]) result
