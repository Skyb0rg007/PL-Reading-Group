open Sem

module Error = struct
    module Type(T : Type1) = struct
        type 'a t =
            Throw of string
          | Catch of 'a T.t * (string -> 'a T.t)
    end

    module Map(F : Functor) = struct
        module T = Type(F)
        let map f = function
              T.Throw e -> T.Throw e
            | T.Catch (m, h) -> T.Catch (F.map f m, fun e -> F.map f (h e))
    end

    module HMap(T : NatTrans) = struct
        module TF = Type(T.F)
        module TG = Type(T.G)
        let map = function
              TF.Throw e -> TG.Throw e
            | TF.Catch (m, h) -> TG.Catch (T.map m, fun e -> T.map (h e))
    end
end

module M = struct
    include Sem(Error)

    let throw e = lift (Throw e)
    let catch m k = lift (Catch (m, k))
end

module EitherMonad = struct
    type 'a t = (string, 'a) Either.t

    let pure = Either.right
    let map = Either.map_right
    let ( >>= ) m f =
        match m with
          Either.Left e -> Either.Left e
        | Either.Right x -> f x
end

module rec HoistEither : sig
    val hoist : 'x Error.Type(M.Self).t -> 'x EitherMonad.t
end = struct
    module T = Error.Type(M.Self)
    let hoist : 'x T.t -> 'x EitherMonad.t = function
          T.Throw e -> Either.Left e
        | T.Catch (m, k) ->
            match FoldEither.to_either m with
              Either.Left e -> FoldEither.to_either (k e)
            | Either.Right a -> Either.Right a
end

and FoldEither : sig
    val to_either : 'a M.t -> 'a EitherMonad.t
end = struct
    module FoldEither = M.Fold(EitherMonad)(HoistEither)
    let to_either : 'a M.t -> 'a EitherMonad.t = FoldEither.fold
end

module Test = struct
    let rec prog n =
        if n < 10
            then M.(>>=) (M.pure n) (fun n -> prog (n + 1))
            else M.throw "error"

    let res = FoldEither.to_either (prog 0)

    let prog2 () =
        M.catch (prog 0)
        (fun e ->
            M.pure ("Caught : " ^ e ^ "!"))

    let res2 = FoldEither.to_either (prog2 ())
end


