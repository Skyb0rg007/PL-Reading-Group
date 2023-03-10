
(* Construct a list of integers, then sum the first 10 elements *)

open! Core
open! Core_bench
open List_optimizations

let args = [100; 10_000; 1_000_000]
(* let args = [100] *)

module Example2(L : ListLike) = struct

    let sum_head =
        let rec loop acc n xs =
            if n <= 0 then acc
            else match L.uncons xs with
              None         -> acc
            | Some (x, xs) -> loop (acc + x) (n - 1) xs
        in loop 0 10

    let tests = [
        Bench.Test.create_indexed
            ~name:"unfold-uncons"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let step n = if n < size then Some (n, n + 1) else None in
                let xs = L.unfold step 0 in
                sum_head xs);
        Bench.Test.create_indexed
            ~name:"cons-uncons"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.cons n acc) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                sum_head xs);
        Bench.Test.create_indexed
            ~name:"append-uncons"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.append acc (L.cons n L.nil)) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                sum_head xs);
    ]
end

let adt_tests =
    let module M = Example2(Adt) in
    Bench.Test.create_group ~name:"Adt" M.tests

let lazy_tests =
    let module M = Example2(Lazy) in
    Bench.Test.create_group ~name:"Lazy" M.tests

let scott_tests =
    let module M = Example2(Scott) in
    Bench.Test.create_group ~name:"Scott" M.tests

let church_tests =
    let module M = Example2(Church) in
    Bench.Test.create_group ~name:"Church" M.tests

let hughes_tests =
    let module M = Example2(Hughes) in
    Bench.Test.create_group ~name:"Hughes" M.tests

let rwr_tests =
    let module M = Example2(Rwr) in
    Bench.Test.create_group ~name:"Rwr" M.tests

let iter_tests =
    let module M = Example2(Iter) in
    Bench.Test.create_group ~name:"Iter" M.tests

let test = Bench.Test.create_group ~name:"Example 2" [
    adt_tests;
    lazy_tests;
    scott_tests;
    church_tests;
    hughes_tests;
    rwr_tests;
    iter_tests;
]
