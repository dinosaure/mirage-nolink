module Either = struct
  type ('a, 'b) t =
    | L of 'a
    | R of 'b

  let map ~l ~r = function
    | L x -> L (l x)
    | R y -> R (r y)

  let left x = L x
  let right x = R x
end

let compose f g x = f (g x)
let identity x = x
let const c _ = c

module type BASE = sig
  type 'a t

  val return : 'a -> 'a t
  val apply : ('a -> 'b) -> 'a t -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
end

module type S = sig
  include BASE

  val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
  val branch : ('a, 'b) Either.t t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t
  val ifS : bool t -> 'a t -> 'a t -> 'a t
  val whenS : bool t -> unit t -> unit t
  val ( <||> ) : bool t -> bool t -> bool t
  val ( <&&> ) : bool t -> bool t -> bool t
end

module Make (Base : BASE) : S with type 'a t = 'a Base.t = struct
  include Base

  let ( <*? ) x f = select x f

  let branch x l r =
    map x ~f:(Either.map ~l:identity ~r:Either.left)
    <*? map l ~f:(compose Either.right)
    <*? r

  let ifS x t f =
    branch
      (map x ~f:(fun b -> if b then Either.left () else Either.right ()))
      (map t ~f:const) (map f ~f:const)

  let whenS x act = ifS x act (return ())

  let ( <||> ) a b = ifS a (return true) b
  let ( <&&> ) a b = ifS a b (return false)
end
