
module type S = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
end

module Identity = struct
    type 'a t = 'a

    let map f x = f x
end

module Const(T : sig type t end) = struct
    type 'a t = T.t

    let map _ x = x
end

module Sum(F : S)(G : S) = struct
    type 'a t = ('a F.t, 'a G.t) Either.t

    let map f = Either.map ~left:(F.map f) ~right:(G.map f)
end

module Product(F : S)(G : S) = struct
    type 'a t = 'a F.t * 'a G.t

    let map f (x, y) = (F.map f x, G.map f y)
end

module Compose(F : S)(G : S) = struct
    type 'a t = 'a G.t F.t

    let map f = F.map (G.map f)
end

