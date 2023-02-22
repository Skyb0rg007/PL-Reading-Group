
module Q = Queue

type 'a t =
    Nil
  | Cons of 'a * 'a t Q.t

(* O(1) *)
let nil = Nil

(* O(1) *)
let cons x xs = Cons (x, Q.singleton xs)

(* Θ(1) *)
let append xs ys =
    match xs with
      Nil         -> ys
    | Cons (x, q) -> Cons (x, Q.snoc q ys)

(* O(n) *)
let unfold step seed =
    let rec loop acc s =
        match step s with
          None         -> acc
        | Some (x, s') -> loop (cons x acc) s'
    in loop nil seed

(* O(n), not stack-safe *)
let rec map f = function
      Nil         -> Nil
    | Cons (x, q) -> let y = f x in Cons (y, Q.map (map f) q)

(* Θ(1) *)
let uncons = function
      Nil         -> None
    | Cons (x, q) ->
        let rec extract q =
            match Q.uncons q with
            | None                    -> Nil
            | Some (Nil, q')          -> extract q'
            | Some (Cons (y, q1), q2) -> Cons (y, Q.append q1 q2)
        in Some (x, extract q)

(* O(n), not stack-safe *)
let rec fold_right f xs z =
    match xs with
      Nil         -> z
    | Cons (x, q) -> f x (Q.fold_right (fold_right f) q z)

(* O(n), not stack-safe *)
let rec fold_left f z = function
      Nil         -> z
    | Cons (x, q) -> Q.fold_left (fold_left f) (f z x) q
