module Step = struct
  type ('s, 'b) t = More of 's | Done of 'b

  let lmap f = function More s -> More (f s) | Done b -> Done b
  let rmap f = function More s -> More s | Done b -> Done (f b)
end

module Void = struct
  type t = |

  let absurd = function (_ : t) -> .
end

type (_, _) t =
  | Reducer : {
      init : ('s, 'b) Step.t;
      step : 'a -> 's -> ('s, 'b) Step.t;
      fin : 's -> 'b;
    }
      -> ('a, 'b) t

let lmap f (Reducer { init; step; fin }) =
  Reducer { init; step = (fun a s -> step (f a) s); fin }

let rmap f (Reducer { init; step; fin }) =
  Reducer
    {
      init = Step.rmap f init;
      step = (fun a s -> Step.rmap f (step a s));
      fin = (fun s -> f (fin s));
    }

let cosieve (Reducer { init; step; fin }) lst =
  let rec loop s = function
    | [] -> fin s
    | x :: xs -> (
        match step x s with Step.Done b -> b | Step.More s' -> loop s' xs)
  in
  match init with Step.Done b -> b | Step.More s -> loop s lst

let pure b =
  Reducer
    { init = Step.Done b; step = (fun _ -> Void.absurd); fin = Void.absurd }

type ('s1, 's2, 'b, 'c) map2_state =
  | NotDone of 's1 * 's2
  | Done1 of 'b * 's2
  | Done2 of 's1 * 'c

let map2_state_merge f s1 s2 =
  match (s1, s2) with
  | Step.Done b, Step.Done c -> Step.Done (f b c)
  | Step.Done b, Step.More s2' -> Step.More (Done1 (b, s2'))
  | Step.More s1', Step.Done c -> Step.More (Done2 (s1', c))
  | Step.More s1', Step.More s2' -> Step.More (NotDone (s1', s2'))

let map2 f (Reducer { init = init1; step = step1; fin = fin1 })
    (Reducer { init = init2; step = step2; fin = fin2 }) =
  Reducer
    {
      init = map2_state_merge f init1 init2;
      step =
        (fun a -> function
          | NotDone (s1, s2) -> map2_state_merge f (step1 a s1) (step2 a s2)
          | Done1 (b, s2) -> map2_state_merge f (Step.Done b) (step2 a s2)
          | Done2 (s1, c) -> map2_state_merge f (step1 a s1) (Step.Done c));
      fin =
        (function
        | NotDone (s1, s2) -> f (fin1 s1) (fin2 s2)
        | Done1 (b, s2) -> f b (fin2 s2)
        | Done2 (s1, c) -> f (fin1 s1) c);
    }

let prefilter p (Reducer { init; step; fin }) =
  Reducer
    { init; step = (fun a s -> if p a then step a s else Step.More s); fin }

let left (Reducer { init; step; fin }) =
  Reducer
    {
      init = Step.rmap (fun x -> `Left x) init;
      step =
        (fun a s ->
          match a with
          | `Left x -> Step.rmap (fun x -> `Left x) (step x s)
          | `Right y -> Step.Done (`Right y));
      fin = (fun s -> `Left (fin s));
    }

let extract (Reducer { init; fin; _ }) =
  match init with Step.Done b -> b | Step.More s -> fin s

let duplicate (Reducer { init; step; fin }) =
  Reducer
    {
      init = Step.rmap pure init;
      step = (fun a s -> Step.rmap pure (step a s));
      fin = (fun s -> Reducer { init = Step.More s; step; fin });
    }

let extend f r = rmap f (duplicate r)

let compose (Reducer { init = init1; step = step1; fin = fin1 })
    (Reducer { init = init2; step = step2; fin = fin2 }) =
  Reducer
    {
      init =
        (match (init1, init2) with
        | Step.Done c, _ -> Step.Done c
        | Step.More s1, Step.More s2 -> Step.More (s1, s2)
        | Step.More s1, Step.Done b -> (
            match step1 b s1 with
            | Step.Done c -> Step.Done c
            | Step.More s1' -> Step.Done (fin1 s1')));
      step =
        (fun a (s1, s2) ->
          match step2 a s2 with
          | Step.Done b -> (
              match step1 b s1 with
              | Step.Done c -> Step.Done c
              | Step.More s1' -> Step.Done (fin1 s1'))
          | Step.More s2' -> (
              match step1 (fin2 s2') s1 with
              | Step.Done c -> Step.Done c
              | Step.More s1' -> Step.More (s1', s2')));
      fin = (fun (s1, _) -> fin1 s1);
    }
