
type 'a t = {run : 'r . 'r -> ('a -> 'r -> 'r) -> 'r}

let nil = {run = fun z _ -> z}

let cons x l = {run = fun z f -> f x (l.run z f)}

let unfold step seed =
    {run = fun z f ->
        let rec loop acc s =
            match step s with
              None         -> acc
            | Some (x, s') -> loop (f x acc) s'
        in loop z seed}

let append l1 l2 = {run = fun z f -> l1.run (l2.run z f) f}

let map g l = {run = fun z f -> l.run z (fun y -> f (g y))}

let fold_right f l z = l.run z f

let fold_left f z l =
    l.run
        (fun acc -> acc)
        (fun x k acc -> f (k acc) x)
        z

let uncons l =
    l.run
        None
        (fun x opt ->
            match opt with
              None         -> Some (x, nil)
            | Some (y, ys) -> Some (x, cons y ys))

