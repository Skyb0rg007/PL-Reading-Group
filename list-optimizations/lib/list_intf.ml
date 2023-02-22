
module type S = sig
    (* Abstract list type *)
    type 'a t

    (* The empty list *)
    val nil : 'a t

    (* Prepend an element to the list *)
    val cons : 'a -> 'a t -> 'a t

    (* Remove the first element from the list *)
    val uncons : 'a t -> ('a * 'a t) option

    (* Concatenate two lists *)
    val append : 'a t -> 'a t -> 'a t

    (* Build a list from a step function and seed value *)
    val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b t

    (* Apply a function to each element of a list *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (* Left-associative fold over a list *)
    val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

    (* Right-associative fold over a list *)
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

