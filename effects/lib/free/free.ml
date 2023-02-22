
module Functor = Functor

module type S = sig
    module F : Functor.S

    type 'a t

    val pure : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val lift : 'a F.t -> 'a t
    val fold : ('a -> 'b) -> ('b F.t -> 'b) -> 'a t -> 'b
    val unwrap : 'a t -> ('a, 'a t F.t) Either.t

    module Infix : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
        val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
        val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    end
end

module Make(F : Functor.S) = struct
    module F = F

    type 'a t =
        Pure of 'a
      | Free of 'a t F.t

    let pure a = Pure a
    
    let rec ( >>= ) m f =
        match m with
          Pure a -> f a
        | Free x -> Free (F.map (Fun.flip (>>=) f) x)

    let lift x = Free (F.map pure x)

    let rec fold f alg = function
          Pure a -> f a
        | Free x -> alg (F.map (fold f alg) x)

    let unwrap = function
          Pure a -> Either.Left a
        | Free b -> Either.Right b

    let both m n = m >>= (fun x -> n >>= (fun y -> pure (x, y)))

    module Infix = struct
        let ( let* ) = ( >>= )
        let ( and* ) = both
        let ( let+ ) m f = m >>= (fun x -> pure (f x))
        let ( and+ ) = both
    end
end

module Cat(F : Functor.S) = struct
    module F = F

    type _ t =
        Pure : 'a -> 'a t
      | Lift : 'a F.t -> 'a t
      | Bind : 'a t * ('a -> 'b t) -> 'b t

    let pure a = Pure a

    let lift t = Lift t

    let ( >>= ) m f = Bind (m, f)

    let both m n = m >>= (fun x -> n >>= (fun y -> pure (x, y)))

    let rec unwrap = function
          Pure a -> Either.Left a
        | Lift t -> Either.Right (F.map pure t)
        | Bind (Pure a, f) -> unwrap (f a)
        | Bind (Lift t, f) -> Either.Right (F.map f t)
        | Bind (Bind (m, g), f) -> unwrap (m >>= (fun x -> g x >>= f))

    let rec fold f alg m =
        match unwrap m with
          Either.Left a -> f a
        | Either.Right b -> alg (F.map (fold f alg) b)

    module Infix = struct
        let ( let* ) = ( >>= )
        let ( and* ) = both
        let ( let+ ) m f = m >>= (fun x -> pure (f x))
        let ( and+ ) = both
    end
end

module Rwr(F : Functor.S) = struct
    module F = F

    module rec Self : sig
        type _ t =
            Free : 'a view * ('a, 'b) Binds.t -> 'b t

        and _ view =
            Pure : 'a -> 'a view
          | Bind : 'a F.t * ('a, 'b) bind -> 'b view

        and ('a, 'b) bind = 'a -> 'b t
    end = struct
        type _ t =
            Free : 'a view * ('a, 'b) Binds.t -> 'b t

        and _ view =
            Pure : 'a -> 'a view
          | Bind : 'a F.t * ('a, 'b) bind -> 'b view

        and ('a, 'b) bind = 'a -> 'b t
    end
    and Bind : sig
        type ('a, 'b) t = ('a, 'b) Self.bind
    end = struct
        type ('a, 'b) t = ('a, 'b) Self.bind
    end
    and Binds : sig
        type ('a, 'b) t

        type (_, _) viewl =
            EmptyL : ('a, 'a) viewl
          | ViewL : ('a, 'b) Self.bind * ('b, 'c) t -> ('a, 'c) viewl

        val empty : ('a, 'a) t
        val snoc : ('a, 'b) t -> ('b, 'c) Self.bind -> ('a, 'c) t
        val viewl : ('a, 'b) t -> ('a, 'b) viewl
        val append : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
    end = Tseq.CatList(Bind)

    include Self

    let pure a = Free (Pure a, Binds.empty)

    let lift t = Free (Bind (t, pure), Binds.empty)

    let ( >>= ) (Free (v, s)) k = Free (v, Binds.snoc s k)

    let concatF (Free (v, l)) r = Free (v, Binds.append l r)

    let rec unwrap : type a. a t -> (a, a t F.t) Either.t =
        fun (Free (v, s)) ->
            match v with
              Bind (f, k) -> Either.Right (F.map (fun a -> concatF (k a) s) f)
            | Pure a ->
                match Binds.viewl s with
                  Binds.EmptyL -> Either.Left a
                | Binds.ViewL (h, t) -> unwrap (concatF (h a) t)

    let rec fold f alg m =
        match unwrap m with
          Either.Left a -> f a
        | Either.Right b -> alg (F.map (fold f alg) b)

    let both m n = m >>= (fun x -> n >>= (fun y -> pure (x, y)))

    module Infix = struct
        let ( let* ) = ( >>= )
        let ( and* ) = both
        let ( let+ ) m f = m >>= (fun x -> pure (f x))
        let ( and+ ) = both
    end
end

module Church(F : Functor.S) = struct
    module F = F

    type 'a t = {run : 'r. ('a -> 'r) -> ('r F.t -> 'r) -> 'r}

    let pure a = {run = fun p _ -> p a}
    let ( >>= ) m f = {run = fun p k -> m.run (fun a -> (f a).run p k) k}
    let lift t = {run = fun p k -> k (F.map p t)}
    let fold f alg m = m.run f alg

    type 'a reified =
        Pure of 'a
      | Free of 'a reified F.t

    let reify m = m.run (fun a -> Pure a) (fun t -> Free t)

    let abstract m =
        {run = fun p k ->
            let rec loop = function
                  Pure a -> p a
                | Free t -> k (F.map loop t)
            in loop m}

    let unwrap m =
        match reify m with
          Pure a -> Either.Left a
        | Free t -> Either.Right (F.map abstract t)

    let both m n = m >>= (fun x -> n >>= (fun y -> pure (x, y)))

    module Infix = struct
        let ( let* ) = ( >>= )
        let ( and* ) = both
        let ( let+ ) m f = m >>= (fun x -> pure (f x))
        let ( and+ ) = both
    end
end


module Codensity(Free : S) = struct
    module F = Free.F

    type 'a t = {run : 'b. ('a -> 'b Free.t) -> 'b Free.t}

    let pure a = {run = fun k -> k a}

    let liftFree m = {run = fun k -> Free.(>>=) m k}
    
    let lower m = m.run Free.pure

    let ( >>= ) m k = {run = fun c -> m.run (fun a -> (k a).run c)}
    let fold f alg m = Free.fold f alg (lower m)
    let lift t = liftFree (Free.lift t)
    let unwrap m =
        match Free.unwrap (lower m) with
          Either.Left a -> Either.Left a
        | Either.Right t -> Either.Right (F.map liftFree t)

    let both m n = m >>= (fun x -> n >>= (fun y -> pure (x, y)))

    module Infix = struct
        let ( let* ) = ( >>= )
        let ( and* ) = both
        let ( let+ ) m f = m >>= (fun x -> pure (f x))
        let ( and+ ) = both
    end

    type 'a cont = {cont : 'b. ('a -> 'b Free.t) -> 'b t}
    let reset m = {run = fun k -> Free.(>>=) (m.run Free.pure) k}
    let shift k = {run = fun h -> (k.cont h).run Free.pure} 
end

