
open! Core
open! Core_bench

let () = Command_unix.run @@ Bench.make_command [
        Example1.test;
        Example2.test;
        Example3.test;
    ]
