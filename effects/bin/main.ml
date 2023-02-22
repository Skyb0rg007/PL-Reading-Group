
let () = print_endline ""

module NonDet = struct
    module F = struct
        type 'a t = Fail | Choose of 'a * 'a

        let map f = function
              Fail -> Fail
            | Choose (a, b) -> Choose (f a, f b)
    end

    module M = Free.Make(F)
    open M.Infix

    let fail () = M.lift F.Fail
    let choose a b = M.lift (F.Choose (a, b))

    let runAll m =
        let alg = function
              F.Fail -> []
            | F.Choose (a, b) -> a @ b
        in M.fold (fun a -> [a]) alg m

    let runFirst =
        M.fold Option.some @@ function
        (* let alg = function *)
              F.Fail -> None
            | F.Choose (None, b) -> b
            | F.Choose (Some _ as a, _) -> a
        (* in M.fold Option.some alg m *)

    let prog () =
        let* a = choose 10 20 in
        let* b = choose 4 15 in
        if a > b
            then M.pure (a + b)
            else fail ()

    let () =
        let res = runAll (prog ()) in
        print_endline @@ "runAll: [" ^ String.concat ", " (List.map Int.to_string res) ^ "]"

    let () =
        let res = runFirst (prog ()) in
        Printf.printf "runFirst: ";
        match res with
          None -> Printf.printf "None\n"
        | Some a -> Printf.printf "Some %d\n" a
end

module State = struct
    module F = struct
        type 'a t =
            Put of int * 'a
          | Get of (int -> 'a)

        let map f = function
              Put (s, k) -> Put (s, f k)
            | Get k -> Get (fun x -> f (k x))
    end

    module M = Free.Make(F)
    open M.Infix

    let get () = M.lift (F.Get Fun.id)
    let put s = M.lift (F.Put (s, ()))

    let runPure m =
        let alg = function
              F.Put (s, k) -> fun _ -> k s
            | F.Get k -> fun s -> k s s
        in M.fold (fun a s -> s, a) alg m 0

    let runMut m =
        let r = ref 0 in
        let alg = function
              F.Put (s, k) -> fun () -> r := s; k ()
            | F.Get k -> let x = !r in fun () -> k x ()
        in
        let x = M.fold (fun a () -> a) alg m () in
        !r, x

    let rec prog () =
        let* n = get () in
        if n < 100 then
            let* () = put (n + 1) in
            prog ()
        else M.pure ()

    let () =
        let (s, ()) = runPure (prog ()) in
        Printf.printf "runPure: %d\n" s

    let () =
        let (s, ()) = runMut (prog ()) in
        Printf.printf "runMut: %d\n" s
end
