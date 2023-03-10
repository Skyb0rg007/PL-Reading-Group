
(* Construct a list of integers, then sum the resulting list *)

open! Core
open! Core_bench
open List_optimizations

(* Step function for creating lists of integers *)
let step size n = if n < size then Some (n, n + 1) else None

let args = [100; 10_000; 1_000_000]
(* let args = [100] *)

module Example1(L : ListLike) = struct
    let tests = [
        Bench.Test.create_indexed
            ~name:"unfold-fold_left"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let xs = L.unfold (step size) 0 in
                L.fold_left (+) 0 xs);
        Bench.Test.create_indexed
            ~name:"cons-fold_left"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.cons n acc) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                L.fold_left (+) 0 xs);
        Bench.Test.create_indexed
            ~name:"append-fold_left"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.append acc (L.cons n L.nil)) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                L.fold_left (+) 0 xs);
        Bench.Test.create_indexed
            ~name:"unfold-fold_right"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let xs = L.unfold (step size) 0 in
                L.fold_right (+) xs 0);
        Bench.Test.create_indexed
            ~name:"cons-fold_right"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.cons n acc) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                L.fold_right (+) xs 0);
        Bench.Test.create_indexed
            ~name:"append-fold_right"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (L.append acc (L.cons n L.nil)) (n + 1)
                        else acc
                in
                let xs = loop L.nil 0 in
                L.fold_right (+) xs 0);
    ]
end

let adt_tests =
    let module M = Example1(Adt) in
    Bench.Test.create_group ~name:"Adt" @@ M.tests @ [
        Bench.Test.create_indexed
            ~name:"unfold-fold_right_safe"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let xs = Adt.unfold (step size) 0 in
                Adt.fold_right_safe (+) xs 0);
        Bench.Test.create_indexed
            ~name:"cons-fold_right_safe"
            ~args
            (fun size -> Staged.stage @@ fun () ->
                let rec loop acc n =
                    if n < size
                        then loop (Adt.cons n acc) (n + 1)
                        else acc
                in
                let xs = loop Adt.nil 0 in
                Adt.fold_right_safe (+) xs 0);
    ]

let lazy_tests =
    let module M = Example1(Lazy) in
    Bench.Test.create_group ~name:"Lazy" M.tests

let scott_tests =
    let module M = Example1(Scott) in
    Bench.Test.create_group ~name:"Scott" M.tests

let church_tests =
    let module M = Example1(Church) in
    Bench.Test.create_group ~name:"Church" M.tests

let hughes_tests =
    let module M = Example1(Hughes) in
    Bench.Test.create_group ~name:"Hughes" M.tests

let rwr_tests =
    let module M = Example1(Rwr) in
    Bench.Test.create_group ~name:"Rwr" M.tests

let iter_tests =
    let module M = Example1(Iter) in
    Bench.Test.create_group ~name:"Iter" M.tests

let test = Bench.Test.create_group ~name:"Example 1" [
    adt_tests;
    lazy_tests;
    scott_tests;
    church_tests;
    hughes_tests;
    rwr_tests;
    iter_tests;
]
