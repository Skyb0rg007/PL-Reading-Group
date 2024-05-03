
type 'a t

type error
type trace

val pure : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : ('a -> 'b) -> 'a t -> 'b t

val run : 'a t -> ('a, trace list * error) Result.t

val string_of_trace : trace -> string
val string_of_error : error -> string

module type Error = sig
    type t

    val to_string : t -> string
end

module MakeError(S : Error) : sig
    val throw : S.t -> 'a t
    val handle : 'a t -> (S.t -> 'a t) -> 'a t
end

module type Trace = sig
    type t

    val to_string : t -> string
end

module MakeTrace(S : Trace) : sig
    val with_context : S.t -> 'a t -> 'a t
end

