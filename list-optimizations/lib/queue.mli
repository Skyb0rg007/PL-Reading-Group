
type 'a t

val empty : 'a t
val singleton : 'a -> 'a t
val snoc : 'a t -> 'a -> 'a t
val uncons : 'a t -> ('a * 'a t) option
val append : 'a t -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
