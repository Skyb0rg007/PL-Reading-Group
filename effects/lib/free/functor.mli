
(* Functor signature *)
module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
end

(* Identity functor *)
module Identity : S with type 'a t = 'a

(* Constant functor *)
module Const(T : sig type t end) : S with type 'a t = T.t

(* The product of functors is a functor *)
module Product(F : S)(G : S) : S with type 'a t = 'a F.t * 'a G.t

(* The composition of functors is a functor *)
module Compose(F : S)(G : S) : S with type 'a t = 'a G.t F.t

(* The disjoint sum of functors is a functor *)
module Sum(F : S)(G : S) : S with type 'a t = ('a F.t, 'a G.t) Either.t
