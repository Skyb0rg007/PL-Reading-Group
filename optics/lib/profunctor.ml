
module type Profunctor = sig

    type ('a, 'b) t

    val lmap : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
    val rmap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

end
