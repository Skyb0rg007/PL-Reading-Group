
(* module type S = sig *)
(*     type ('a, 'b) t *)
(*     type ('a, 'b) elem *)

(*     type ('a, 'b) viewl = *)
(*         EmptyL : ('a, 'a) viewl *)
(*       | ConsL : ('a, 'b) elem * ('b, 'c) t -> ('a, 'c) viewl *)

(*     type ('a, 'b) viewr = *)
(*         EmptyR : ('a, 'a) viewr *)
(*       | SnocR : ('a, 'b) t * ('b, 'c) elem -> ('a, 'c) viewr *)

(*     val empty : ('a, 'a) t *)

(*     val cons : ('a, 'b) elem -> ('b, 'c) t -> ('a, 'c) t *)
    
(*     val snoc : ('a, 'b) t -> ('b, 'c) elem -> ('a, 'c) t *)

(*     val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'b) t *)

(*     val viewl : ('a, 'b) t -> ('a, 'b) viewl *)

(*     val viewr : ('a, 'b) t -> ('a, 'b) viewr *)
(* end *)

module ConsList(T : sig type ('a, 'b) t end) = struct
    type (_, _) t =
        Nil : ('a, 'a) t
      | Cons : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) t

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    let empty = Nil

    let cons x xs = Cons (x, xs)

    let viewl : type a b. (a, b) t -> (a, b) viewl = function
          Nil -> EmptyL
        | Cons (x, xs) -> ViewL (x, xs)

    let [@tail_mod_cons] rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
        fun xs ys ->
            match xs with
              Nil -> ys
            | Cons (x, xs) -> Cons (x, append xs ys)
end

module SnocList(T : sig type ('a, 'b) t end) = struct
    type (_, _) t =
        Nil : ('a, 'a) t
      | Snoc : ('a, 'b) t * ('b, 'c) T.t -> ('a, 'c) t

    type (_, _) viewr =
        EmptyR : ('a, 'a) viewr
      | ViewR : ('a, 'b) t * ('b, 'c) T.t -> ('a, 'c) viewr

    let empty = Nil

    let snoc xs x = Snoc (xs, x)

    let viewr : type a b. (a, b) t -> (a, b) viewr = function
          Nil -> EmptyR
        | Snoc (xs, x) -> ViewR (xs, x)

    let [@tail_mod_cons] rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
        fun xs ys ->
            match ys with
              Nil -> xs
            | Snoc (ys, y) -> Snoc (append xs ys, y)

    module C = ConsList(T)

    let rec rev_append : type a b c. (a, b) t -> (b, c) C.t -> (a, c) C.t =
        fun xs ys ->
            match xs with
              Nil -> ys
            | Snoc (xs, x) -> rev_append xs (C.cons x ys)

    let reverse xs = rev_append xs C.empty
end

module Queue(T : sig type ('a, 'b) t end) = struct
    module C = ConsList(T)
    module S = SnocList(T)

    type (_, _) t =
        Q : ('a, 'b) C.t * ('b, 'c) S.t -> ('a, 'c) t

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    let empty = Q (C.empty, S.empty)

    let snoc (Q (l, r)) x = Q (l, S.snoc r x)

    let rec viewl : type a b. (a, b) t -> (a, b) viewl =
        fun (Q (l, r)) ->
            match C.viewl l, S.viewr r with
              C.EmptyL, S.EmptyR -> EmptyL
            | C.EmptyL, _        -> viewl (Q (S.reverse r, S.empty))
            | C.ViewL (x, xs), _ -> ViewL (x, Q (xs, r))

    let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
        fun xs ys ->
            match viewl ys with
              EmptyL -> xs
            | ViewL (y, ys) -> append (snoc xs y) ys
end

module CatList(T : sig type ('a, 'b) t end) = struct
    module rec Self : sig
        type (_, _) t =
            Nil : ('a, 'a) t
          | Cons : ('a, 'b) T.t * ('b, 'c) Q.t -> ('a, 'c) t
    end = struct
        type (_, _) t =
            Nil : ('a, 'a) t
          | Cons : ('a, 'b) T.t * ('b, 'c) Q.t -> ('a, 'c) t
    end
    and Q : sig
        type ('a, 'b) t

        type (_, _) viewl =
            EmptyL : ('a, 'a) viewl
          | ViewL : ('a, 'b) Self.t * ('b, 'c) t -> ('a, 'c) viewl

        val empty : ('a, 'a) t
        val snoc : ('a, 'b) t -> ('b, 'c) Self.t -> ('a, 'c) t
        val viewl : ('a, 'b) t -> ('a, 'b) viewl
        val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
    end = Queue(Self)

    include Self

    type (_, _) viewl =
        EmptyL : ('a, 'a) viewl
      | ViewL : ('a, 'b) T.t * ('b, 'c) t -> ('a, 'c) viewl

    let empty = Nil

    let append (type a b c) (xs : (a, b) t) (ys : (b, c) t) : (a, c) t =
        match xs, ys with
          Nil, _ -> ys
        | _, Nil -> xs
        | Cons (a, q), _ -> Cons (a, Q.snoc q ys)

    let cons (type a b c) (x : (a, b) T.t) (xs : (b, c) t) : (a, c) t =
        append (Cons (x, Q.empty)) xs

    let snoc (type a b c) (xs : (a, b) t) (x : (b, c) T.t) : (a, c) t =
        append xs (Cons (x, Q.empty))

    let viewl : type a b. (a, b) t -> (a, b) viewl = function
          Nil -> EmptyL
        | Cons (a, q) ->
            match Q.viewl q with
              Q.EmptyL -> ViewL (a, Nil)
            | Q.ViewL (Nil, _) -> raise (Failure "Nil in queue")
            | Q.ViewL (Cons (b, q1), q2) -> ViewL (a, Cons (b, Q.append q1 q2))

end
