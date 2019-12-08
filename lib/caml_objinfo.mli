module type S = sig
  type t

  val name : t -> string
  val iface_digest : t -> Digest.t
  val iface_deps : t -> (string * Digest.t option) list
end

module CMI : S
module CMO : S
module CMT : S
module CMTI : S

module CMA : sig
  type t

  val name : t -> string
  val cmos : t -> CMO.t list
  val custom : t -> bool
  val custom_cobjs : t -> string list
  val custom_copts : t -> string list
  val dllibs : t -> string list
end

module CMX : sig
  include S

  val digest : t -> Digest.t
  val cmx_deps : t -> (string * Digest.t option) list
end

module CMXA : sig
  type t

  val name : t -> string
  val cmxs : t -> CMX.t list
  val cobjs : t -> string list
  val copts : t -> string list
end

type t =
  | CMO of CMO.t
  | CMI of CMI.t
  | CMX of CMX.t
  | CMA of CMA.t
  | CMT of CMT.t
  | CMXA of CMXA.t
  | CMTI of CMTI.t

type error =
  [ `Not_found of Fpath.t
  | `Invalid_object of Fpath.t
  | `Msg of string ]

val pp_error : error Fmt.t
val info : Fpath.t -> (t, [> error ]) result
