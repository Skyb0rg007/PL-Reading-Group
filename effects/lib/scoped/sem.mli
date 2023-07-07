
module type Type1 = sig
    type 'a t
end

module type Functor = sig
    include Type1

    (* Should satisfy:
     *   map id == id
     *   map (f |> g) == map f |> map g
     *)
    val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Monad = sig
    include Functor

    val pure : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type NatTrans = sig
    module F : Functor
    module G : Functor

    (* Should satisfy:
     *   F.map f |> map == map |> G.map f
     *)
    val map : 'a F.t -> 'a G.t
end

module type HFunctor = sig
    module Type(_ : Type1) : Type1

    module Map(F : Functor) : sig
        val map : ('a -> 'b) -> 'a Type(F).t -> 'b Type(F).t
    end

    module HMap(T : NatTrans) : sig
        val map : 'a Type(T.F).t -> 'a Type(T.G).t
    end
end

module Sem(H : HFunctor) : sig
    module Self : sig
        type 'a t
    end

    type 'a t = 'a Self.t

    val pure : 'a -> 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val lift : 'a H.Type(Self).t -> 'a t

    module Fold(M : Monad)(_ : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) : sig
        val fold : 'a t -> 'a M.t
    end
end
