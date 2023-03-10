
type 'a t = unit -> 'a node

and 'a node =
    Nil
  | Cons of 'a * 'a t


let nil () = Nil

let cons x xs () = Cons (x, xs)

let uncons l =
    match l () with
      Nil          -> None
    | Cons (x, xs) -> Some (x, xs)

let rec append seq1 seq2 () =
    match seq1 () with
      Nil -> seq2 ()
    | Cons (x, next) -> Cons (x, append next seq2)

let rec unfold f u () =
    match f u with
      None -> Nil
    | Some (x, u') -> Cons (x, unfold f u')

let rec map f seq () =
    match seq () with
      Nil -> Nil
    | Cons (x, next) -> Cons (f x, map f next)

let rec fold_left f acc seq =
    match seq () with
      Nil -> acc
    | Cons (x, next) -> fold_left f (f acc x) next

let fold_right f seq acc =
    List.fold_left (Fun.flip f) acc (fold_left (Fun.flip List.cons) [] seq)
