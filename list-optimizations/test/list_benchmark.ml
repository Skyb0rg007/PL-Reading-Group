
open! Core
open! Core_bench
open List_optimizations

let step size n = if n < size then Some (n, n + 1) else None

let () = Command_unix.run @@ Bench.make_command [
        (* Bench.Test.create_indexed *)
        (*     ~name:"Adt unfold-fold_left" *)
        (*     ~args:[100; 1_000; 10_000; 100_000] *)
        (*     (fun size -> Staged.stage @@ fun () -> *)
        (*         let xs = Adt.unfold (step size) 0 in *)
        (*         Adt.fold_left (+) 0 xs); *)
        (* Bench.Test.create_indexed *)
        (*     ~name:"Church unfold-fold_right" *)
        (*     ~args:[100; 1_000; 10_000; 100_000] *)
        (*     (fun size -> Staged.stage @@ fun () -> *)
        (*         let xs = Church.unfold (step size) 0 in *)
        (*         Church.fold_right (+) xs 0); *)

        Bench.Test.create_indexed
            ~name:"Adt unfold-append-fold_left"
            ~args:[100; 1_000; 10_000; 100_000]
            (fun size -> Staged.stage @@ fun () ->
                let xs = Adt.unfold (step size) 0 in
                let xs = Adt.append xs xs in
                let xs = Adt.append xs xs in
                Adt.fold_left (+) 0 xs);
        Bench.Test.create_indexed
            ~name:"Church unfold-append-fold_right"
            ~args:[100; 1_000; 10_000; 100_000]
            (fun size -> Staged.stage @@ fun () ->
                let xs = Church.unfold (step size) 0 in
                let xs = Church.append xs xs in
                let xs = Church.append xs xs in
                Church.fold_right (+) xs 0);
        Bench.Test.create_indexed
            ~name:"Hughes unfold-append-fold_left"
            ~args:[100; 1_000; 10_000; 100_000]
            (fun size -> Staged.stage @@ fun () ->
                let xs = Hughes.unfold (step size) 0 in
                let xs = Hughes.append xs xs in
                let xs = Hughes.append xs xs in
                Hughes.fold_left (+) 0 xs);
    ]
