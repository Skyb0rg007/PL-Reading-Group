
(* Starting with a small list, repeatedly appends elements then reads the head *)

open! Core
open! Core_bench
open List_optimizations

let args = [100; 1_000; 10_000]
(* let args = [10_000] *)

module Example3(L : ListLike) = struct

    let step size n = if n < size then Some (n, n + 1) else None

    let tests = [
        Bench.Test.create_indexed
            ~name:"append-uncons"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let small = L.cons 1 (L.cons 2 L.nil) in
                let rec loop n l =
                    if n <= 0 then -100 else
                    match L.uncons l with
                      None -> failwith "Reached end of list!"
                    | Some (_, rest) -> loop (n - 1) (L.append rest small)
                in loop size (L.unfold (step 10) 0))
    ]

end

let adt_tests =
    let module M = Example3(Adt) in
    Bench.Test.create_group ~name:"Adt" M.tests

let lazy_tests =
    let module M = Example3(Lazy) in
    Bench.Test.create_group ~name:"Lazy" M.tests

let scott_tests =
    let module M = Example3(Scott) in
    Bench.Test.create_group ~name:"Scott" M.tests

let church_tests =
    let module M = Example3(Church) in
    Bench.Test.create_group ~name:"Church" M.tests

let hughes_tests =
    let module M = Example3(Hughes) in
    Bench.Test.create_group ~name:"Hughes" M.tests

let rwr_tests =
    let module M = Example3(Rwr) in
    Bench.Test.create_group ~name:"Rwr" M.tests

let iter_tests =
    let module M = Example3(Iter) in
    Bench.Test.create_group ~name:"Iter" M.tests

let test = Bench.Test.create_group ~name:"Example 3" [
    adt_tests;
    lazy_tests;
    scott_tests;
    church_tests;
    hughes_tests;
    rwr_tests;
    iter_tests;
]

