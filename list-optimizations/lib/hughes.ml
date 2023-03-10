
type 'a t = 'a list -> 'a list

let nil r = r

let cons x k r = x :: k r

let append k1 k2 r = k1 (k2 r)

let unfold step seed r =
    let rec loop acc s =
        match step s with
          None -> acc
        | Some (x, s') -> loop (x :: acc) s'
    in loop r seed

let fold_right f k z = List.fold_right f (k []) z

let fold_left f z k = List.fold_left f z (k [])

let map f k r = List.map f (k []) @ r

let uncons k =
    match k [] with
    | [] -> None
    | x :: rest -> Some (x, fun r -> rest @ r)

