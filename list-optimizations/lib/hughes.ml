
type 'a t = 'a list -> 'a list

(* O(1) *)
let nil r = r

(* O(1), not stack-safe *)
let cons x k r = x :: k r

(* O(1) *)
let append k1 k2 r = k1 (k2 r)

(* O(n) *)
let unfold step seed r =
    let rec loop acc s =
        match step s with
          None -> acc
        | Some (x, s') -> loop (x :: acc) s'
    in loop r seed

(* O(n), not stack-safe *)
let fold_right f k z = List.fold_right f (k []) z

(* O(n) *)
let fold_left f z k = List.fold_left f z (k [])

(* O(n) *)
let map f k r = List.map f (k []) @ r

(* O(n) *)
let uncons k =
    match k [] with
      []      -> None
    | x :: xs -> Some (x, fun r -> xs @ r)

