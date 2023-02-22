
type 'a t =
    Nil
  | Cons of 'a * 'a t

(* O(1) *)
let nil = Nil

(* O(1) *)
let cons x xs = Cons (x, xs)

(* O(1) *)
let uncons = function
      Nil          -> None
    | Cons (x, xs) -> Some (x, xs)

(* O(n), not stack-safe *)
let rec append xs ys =
    match xs with
      Nil          -> ys
    | Cons (x, xs) -> Cons (x, append xs ys)

(* O(n) *)
let unfold step seed =
    let rec loop acc s =
        match step s with
          None         -> acc
        | Some (x, s') -> loop (Cons (x, acc)) s'
    in loop Nil seed

(* O(n), not stack safe *)
let rec map f = function
      Nil          -> Nil
    | Cons (x, xs) -> let y = f x in Cons (y, map f xs)

(* O(n) *)
let rec fold_left f z = function
      Nil          -> z
    | Cons (x, xs) -> fold_left f (f z x) xs

(* O(n), not stack safe *)
let rec fold_right f l z =
    match l with
      Nil          -> z
    | Cons (x, xs) -> f x (fold_right f xs z)

