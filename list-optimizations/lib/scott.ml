
type 'a t = {run : 'r. 'r -> ('a -> 'a t -> 'r) -> 'r}

(* O(1) *)
let nil = {run = fun n _ -> n}

(* O(1) *)
let cons x xs = {run = fun _ c -> c x xs}

(* O(1) *)
let uncons xs = xs.run None (fun y ys -> Some (y, ys))

(* O(n) *)
let unfold step seed =
    let rec loop acc s =
        match step s with
          None -> acc
        | Some (x, s') -> loop (cons x acc) s'
    in loop nil seed

(* O(n), not stack-safe *)
let rec append xs ys =
    xs.run
        ys
        (fun x xs -> cons x (append xs ys))

(* O(n), not stack-safe *)
let rec map f xs =
    xs.run
        nil
        (fun x xs -> cons (f x) (map f xs))

(* O(n) *)
let rec fold_left f z xs =
    xs.run
        z
        (fun x xs -> fold_left f (f z x) xs)

(* O(n), not stack-safe *)
let rec fold_right f xs z =
    xs.run
        z
        (fun x xs -> f x (fold_right f xs z))
