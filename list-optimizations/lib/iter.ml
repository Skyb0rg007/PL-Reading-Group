
type 'a t =
    T : 's * ('s -> ('a * 's) option) -> 'a t

let nil = T ((), fun () -> None)

let cons x (T (seed, step)) =
    let step' = function
          None   -> Some (x, Some seed)
        | Some s ->
            match step s with
              None         -> None
            | Some (x, s') -> Some (x, Some s')
    in T (None, step')

let uncons (T (seed, step)) =
    match step seed with
      None        -> None
    | Some (x, s) -> Some (x, T (s, step))

let append (T (seed1, step1)) (T (seed2, step2)) =
    let rec step' = function
          Either.Left s1 ->
            begin match step1 s1 with
              None          -> step' (Either.Right seed2)
            | Some (x, s1') -> Some (x, Either.Left s1')
            end
        | Either.Right s2 ->
            match step2 s2 with
              None          -> None
            | Some (y, s2') -> Some (y, Either.Right s2')
    in T (Either.Left seed1, step')

let unfold step seed = T (seed, step)

let map f (T (seed, step)) =
    let step' s =
        match step s with
          None         -> None
        | Some (x, s') -> Some (f x, s')
    in T (seed, step')

let fold_left f z (T (seed, step)) =
    let rec loop acc s =
        match step s with
          None         -> acc
        | Some (x, s') -> loop (f acc x) s'
    in loop z seed

let fold_right f xs z =
    List.fold_left (Fun.flip f) z (fold_left (Fun.flip List.cons) [] xs)



