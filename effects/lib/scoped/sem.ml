
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

module type Monad = sig
    include Functor

    val pure : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Sem(H : HFunctor) = struct

    module rec Self : sig
        module type S = sig
            type t

            module F(M : Monad)(_ : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) : sig
                val x : t M.t
            end
        end

        type 'a t = (module S with type t = 'a)
    end = struct
        module type S = sig
            type t

            module F(M : Monad)(_ : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) : sig
                val x : t M.t
            end
        end

        type 'a t = (module S with type t = 'a)
    end

    include Self

    let pure (type a) (a : a) : a t =
        let module A = struct
            type t = a
            module F(M : Monad)(_ : sig end) = struct
                let x = M.pure a
            end
        end
        in (module A)

    let ( >>= ) (type a b) (m : a t) (k : a -> b t) : b t =
        let module A = (val m : S with type t = a) in
        let module B = struct
            type t = b
            module F(M : Monad)(X : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) = struct
                module AMX = A.F(M)(X)
                let x = M.(>>=) AMX.x
                    (fun z ->
                        let kz = k z in
                        let module KZ = (val kz : S with type t = b) in
                        let module R = KZ.F(M)(X) in
                        R.x)
            end
        end
        in (module B)

    let lift (type a) (u : a H.Type(Self).t) : a t =
        let module A = struct
            type t = a
            module F(M : Monad)(X : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) = struct
                let x = X.hoist u
            end
        end
        in (module A)

    module Fold(M : Monad)(X : sig val hoist : 'x H.Type(Self).t -> 'x M.t end) = struct
        let fold (type a) (m : a t) : a M.t =
            let module A = (val m : S with type t = a) in
            let module AM = A.F(M) in
            let module AMX = AM(X) in
            AMX.x
    end
end

