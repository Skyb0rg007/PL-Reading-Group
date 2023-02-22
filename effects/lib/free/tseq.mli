
(* Type-aligned sequences *)

module ConsList(T : sig type ('a, 'b) t end) : sig
    type ('a, 'b) t

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    val empty : ('a, 'a) t
    val cons : ('a, 'b) T.t -> ('b, 'c) t -> ('a, 'c) t
    val viewl : ('a, 'b) t -> ('a, 'b) viewl
    val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
end

module SnocList(T : sig type ('a, 'b) t end) : sig
    type ('a, 'b) t

    type (_, _) viewr =
        EmptyR : ('a, 'a) viewr
      | ViewR : ('a, 'b) t * ('b, 'c) T.t -> ('a, 'c) viewr

    val empty : ('a, 'a) t
    val snoc : ('a, 'b) t -> ('b, 'c) T.t -> ('a, 'c) t
    val viewr : ('a, 'b) t -> ('a, 'b) viewr
    val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
    val reverse : ('a, 'b) t -> ('a, 'b) ConsList(T).t
end

module Queue(T : sig type ('a, 'b) t end) : sig
    type ('a, 'b) t

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    val empty : ('a, 'a) t
    val snoc : ('a, 'b) t -> ('b, 'c) T.t -> ('a, 'c) t
    val viewl : ('a, 'b) t -> ('a, 'b) viewl
    val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
end

(* All operations are amortized O(1) *)
module CatList(T : sig type ('a, 'b) t end) : sig
    type ('a, 'b) t

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    val empty : ('a, 'a) t
    val cons : ('a, 'b) T.t -> ('b, 'c) t -> ('a, 'c) t
    val snoc : ('a, 'b) t -> ('b, 'c) T.t -> ('a, 'c) t
    val viewl : ('a, 'b) t -> ('a, 'b) viewl
    val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
end
