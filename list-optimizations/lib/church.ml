
type 'a t = {run : 'r . 'r -> ('a -> 'r -> 'r) -> 'r}

(* O(1) *)
let nil = {run = fun z _ -> z}

(* O(1), not stack-safe *)
let cons x xs = {run = fun z f -> f x (xs.run z f)}

(* O(n) *)
let unfold step seed =
    {run = fun z f ->
        let rec loop acc s =
            match step s with
              None         -> acc
            | Some (x, s') -> loop (f x acc) s'
        in loop z seed}

(* O(1) *)
let append xs ys = {run = fun z f -> xs.run (ys.run z f) f}

(* O(1) *)
let map g xs = {run = fun z f ->
    xs.run z (fun y ys -> f (g y) ys)}

(* O(n) *)
let fold_right f xs z = xs.run z f

(* O(n) *)
let fold_left f z xs =
    xs.run
        (fun acc -> acc)
        (fun x k acc -> f (k acc) x)
        z

(* O(n) *)
let uncons xs =
    xs.run
        None
        (fun x opt ->
            match opt with
              None         -> Some (x, nil)
            | Some (y, ys) -> Some (x, cons y ys))

