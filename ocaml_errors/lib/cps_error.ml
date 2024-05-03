
type error = ..
type trace = ..

type 'a t = { run : 'r. ('a -> 'r) -> (trace list -> error -> 'r) -> 'r }

let pure x = { run = fun ok _err -> ok x }

let map f m = { run = fun ok err -> m.run (fun a -> ok (f a)) err }

let bind m k = { run = fun ok err -> m.run (fun a -> (k a).run ok err) err }

let run m = m.run Result.ok (fun trace error -> Result.error (trace, error))

let ( let* ) = bind
let ( let+ ) = map

let string_of_trace' = ref (fun _ -> failwith "impossible")
let string_of_error' = ref (fun _ -> failwith "impossible")

let string_of_trace t = !string_of_trace' t
let string_of_error e = !string_of_error' e

module type Error = sig
    type t

    val to_string : t -> string
end

module MakeError(S : Error) = struct
    type error += E of S.t

    let () =
        let old = !string_of_error' in
        string_of_error' := function
        | E e -> S.to_string e
        | e -> old e

    let throw e = { run = fun _ok err -> err [] (E e) }

    let handle m h = { run = fun ok err ->
        m.run ok (fun trace -> function
            | E e -> (h e).run ok err
            | e -> err trace e) }
end

module type Trace = sig
    type t

    val to_string : t -> string
end

module MakeTrace(S : Trace) = struct
    type trace += T of S.t

    let () =
        let old = !string_of_trace' in
        string_of_trace' := function
        | T e -> S.to_string e
        | e -> old e

    let with_context t m = { run = fun ok err ->
        m.run ok (fun trace e -> err (T t :: trace) e) }
end
