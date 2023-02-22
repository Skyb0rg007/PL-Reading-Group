
open! Free

(* Error functor *)
module ErrorF = struct
    type 'a t =
        Throw of string

    let map _ = function
          Throw e -> Throw e
end

(* State functor *)
module StateF = struct
    type 'a t =
        Get of (int -> 'a)
      | Put of int * 'a

    let map f = function
          Get k -> Get (fun n -> f (k n))
        | Put (n, k) -> Put (n, f k)
end

(* We can take the arbitrary sum of functors *)
module ErrorStateF = Functor.Sum(ErrorF)(StateF)

(* Free monad over this functor sum *)
module FreeErrorState = struct
    (* Trying out other free monad representations *)
    include Free.Make(ErrorStateF)
    (* include Free.Cat(ErrorStateF) *)
    (* include Free.Church(ErrorStateF) *)
    (* include Free.Rwr(ErrorStateF) *)
    (* include Free.Codensity(Free.Make(ErrorStateF)) *)
    (* include Free.Codensity(Free.Cat(ErrorStateF)) *)
    (* include Free.Codensity(Free.Rwr(ErrorStateF)) *)

    let throw e = lift (Either.Left (ErrorF.Throw e))
    let get () = lift (Either.Right (StateF.Get Fun.id))
    let put n = lift (Either.Right (StateF.Put (n, ())))

    let runAbortive = fold (fun a s -> Either.Right (s, a)) @@ function
          Either.Left (ErrorF.Throw e) -> fun _ -> Either.Left e
        | Either.Right (StateF.Get k) -> fun s -> k s s
        | Either.Right (StateF.Put (s, k)) -> fun _ -> k s

    let runSaved = fold (fun a s -> s, Either.Right a) @@ function
          Either.Left (ErrorF.Throw e) -> fun s -> s, Either.Left e
        | Either.Right (StateF.Get k) -> fun s -> k s s
        | Either.Right (StateF.Put (s, k)) -> fun _ -> k s

    let _ : 'a t -> int -> (string, int * 'a) Either.t = runAbortive
    let _ : 'a t -> int -> int * (string, 'a) Either.t = runSaved
end

(* OCaml effect system *)
module EffErrorState = struct
    type _ Effect.t += Throw : string -> 'a Effect.t
    type _ Effect.t += Get : int Effect.t
    type _ Effect.t += Put : int -> unit Effect.t

    type 'a t = 'a

    let pure a = a
    let ( >>= ) m f = f m

    let throw e = Effect.perform (Throw e)
    let get () = Effect.perform Get
    let put n = Effect.perform (Put n)

    let run (s : int) f =
        let s = ref s in
        let x = Effect.Deep.try_with f ()
            { effc = fun (type a) (eff : a Effect.t) ->
                match eff with
                  Throw e -> Some (fun (k : (a, _) Effect.Deep.continuation) -> Effect.Deep.discontinue k (Failure e))
                | Get -> Some (fun k -> Effect.Deep.continue k !s)
                | Put n -> Some (fun k -> s := n; Effect.Deep.continue k ())
                | _ -> None }
        in !s, x
end

module EffFree(M : Free.S) = struct
    type _ Effect.t += F : 'a M.F.t -> 'a Effect.t

    type 'a t = unit -> 'a
    let pure a () = a
    let ( >>= ) m f () = f (m ()) ()

    let of_free m () = M.fold pure (fun t -> Effect.perform (F t)) m ()

    (* XXX: Is this correct? *)
    let to_free m =
        Effect.Deep.try_with (fun () -> M.pure (m ())) ()
        { effc = fun (type a) (eff : a Effect.t) ->
            match eff with
              F (t : a M.F.t) -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
                (* Cleanup after continuations which are never continued *)
                let exception Unwind in
                let cleanup k = try ignore (Effect.Deep.discontinue k Unwind) with _ -> () in
                Gc.finalise cleanup k;
                M.(>>=) (M.lift t) (Effect.Deep.continue k)
            | _ -> None }

    let _ : 'a M.t -> 'a t = of_free
    let _ : 'a t -> 'a M.t = to_free
end

module EffErrorState2 = struct
    include EffFree(FreeErrorState)

    let throw e = of_free (FreeErrorState.throw e)
    let get () = of_free (FreeErrorState.get ())
    let put n = of_free (FreeErrorState.put n)
end

(* General module type (tagless final) *)
module type S = sig
    type 'a t

    val pure : 'a -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val throw : string -> 'a t
    val get : unit -> int t
    val put : int -> unit t
end

module Test(M : S) = struct
    open! M
    let ( let* ) = ( >>= )

    let rec prog1 () =
        let* n = get () in
        if n < 0
            then throw "Negative number"
        else if n < 100
            then let* _ = put (n + 1) in prog1 ()
        else pure ()
end

module A = Test(FreeErrorState)

let () =
    print_endline "Free: runAbortive prog1";
    match FreeErrorState.runAbortive (A.prog1 ()) 0 with
      Either.Left e -> print_endline ("Left " ^ e)
    | Either.Right (n, ()) -> print_endline ("Right (" ^ Int.to_string n ^ ", ())")

let () =
    print_endline "Free: runSaved prog1";
    match FreeErrorState.runSaved (A.prog1 ()) 0 with
      (n, Either.Left e) -> print_endline ("(" ^ Int.to_string n ^ ", Left " ^ e ^ ")")
    | (n, Either.Right ()) -> print_endline ("(" ^ Int.to_string n ^ ", Right ())")

module B = Test(EffErrorState)

let () =
    print_endline "Effect: run prog1";
    match EffErrorState.run 0 B.prog1 with
      exception Failure e -> print_endline ("Failure - " ^ e)
    | (n, ()) -> print_endline ("(" ^ Int.to_string n ^ ", ())")

module C = Test(EffErrorState2)

let () =
    print_endline "Effect2: run prog1";
    match FreeErrorState.runSaved (EffErrorState2.to_free (C.prog1 ())) 0 with
      (n, Either.Left e) -> print_endline ("(" ^ Int.to_string n ^ ", Left " ^ e ^ ")")
    | (n, Either.Right ()) -> print_endline ("(" ^ Int.to_string n ^ ", Right ())")

