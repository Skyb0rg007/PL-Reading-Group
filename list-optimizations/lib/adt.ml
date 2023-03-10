
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

(** Tail-recursion modulo cons versions *)

let [@tail_mod_cons] rec append_trmc xs ys =
    match xs with
      Nil          -> ys
    | Cons (x, xs) -> Cons (x, append_trmc xs ys)

let [@tail_mod_cons] rec map_trmc f = function
      Nil          -> Nil
    | Cons (x, xs) -> let y = f x in Cons (y, map_trmc f xs)

(** Stack-safe versions *)

(* `rev_append xs ys` is equivalent to `append (rev xs) ys` *)
let rec rev_append xs ys =
    match xs with
      Nil          -> ys
    | Cons (x, xs) -> rev_append xs (Cons (x, ys))

let rev xs = rev_append xs Nil

let append_safe xs ys = rev_append (rev xs) ys

let map_safe f xs =
    let rec loop acc = function
          Nil          -> acc
        | Cons (x, xs) -> let y = f x in loop (Cons (y, acc)) xs
    in
    loop (rev xs)

let fold_right_safe f l z = fold_left (Fun.flip f) z (rev l)
