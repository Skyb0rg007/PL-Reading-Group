
module Functor = Functor

module type S = sig
    module F : Functor.S

    type 'a t

    (* Monadic operations *)
    val pure : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    (* Lift the functor into the monad *)
    val lift : 'a F.t -> 'a t

    (* Free monad elimination *)
    val fold : ('a -> 'b) -> ('b F.t -> 'b) -> 'a t -> 'b
    val unwrap : 'a t -> ('a, 'a t F.t) Either.t

    (* Monadic syntax. To be `open`ed. *)
    module Infix : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
        val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
        val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    end
end

(* Standard free monad
 * Inefficient ( >>= ), but efficient unwrap
 *)
module Make(F : Functor.S) : S with module F = F

(* Free monad with delayed binds, used by Scala Cats
 * Not sure what the performance characteristics are
 *)
module Cat(F : Functor.S) : S with module F = F

(* Reflection-without-remorse, used by purescript-free
 * "Reflection without Remorse"
 *   by Atze van der Ploeg and Oleg Kiselyov
 * https://okmij.org/ftp/Haskell/zseq.pdf
 * Trades some inefficiency with unwrap for ( >>= ) efficiency
 * Higher constant factors, very complex implementation
 *)
module Rwr(F : Functor.S) : S with module F = F

(* Church-encoded free monad
 * "Free Monads for Less"
 *   by Edward Kmett
 * https://ekmett.github.io/reader/2011/free-monads-for-less-2/index.html
 * Efficient ( >>= ) but inefficient unwrap
 *)
module Church(F : Functor.S) : S with module F = F

(* Codensity transform
 * "Asymptotic Improvement of Computations over Free Monads"
 *   by Janis Voigtländer
 * https://janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf
 * Efficient ( >>= ) but inefficient unwrap
 * This adds delimited continuation capability
 *)
module Codensity(Free : S) : sig
    include S with module F = Free.F

    val lower : 'a t -> 'a Free.t
    val liftFree : 'a Free.t -> 'a t

    type 'a cont = {cont : 'b. ('a -> 'b Free.t) -> 'b t}

    val reset : 'a t -> 'a t
    val shift : 'a cont -> 'a t
end

