open Mattlude.Misc

module Repl = struct
  type 'next t =
    | Getline of (string -> 'next)
    | Quit
    | Response of string * 'next
    | Forever of 'next

  let map f = function
    | Getline cont -> Getline (fun x -> cont x |> f)
    | Response (string, next) -> Response (string, f next)
    | Quit -> Quit
    | Forever action -> Forever (f action)
end

module ReplInterpreter = struct
  module FRepl = Free.Make (Repl)
  include FRepl

  let get_line () = lift (Getline Fun.id) 
  let response msg = lift (Response (msg, ()))
  let quit () = lift Quit
  let forever action = Join (Forever action)

  let main =
    let one_round =
      let* input = get_line () in
      let msg =
        Printf.sprintf "You just typed %s!" input
      in
      if input = "quit"
      then quit ()
      else response msg
    in
    forever one_round

  let optimize = function
    | Join (Response (msg1, Join Response (msg2, next))) ->
       Join (Response (msg1 ^ msg2, next))
    | other -> other
end

module Executable = struct
  open ReplInterpreter

  module Real = struct
    let rec real_repl = function
      | Pure next -> next
      | Join Getline cont ->
         let input = Stdlib.read_line ()
         in input |> cont |> real_repl
      | Join Quit -> print_endline "Exiting!" ;
                     Stdlib.exit 0
      | Join Response (msg, next) ->
         print_endline msg ;
         real_repl next
      | Join Forever action ->
         while true do real_repl action |> ignore done
    let main () = real_repl main
  end

  module Dry = struct
    let rec dry_run =
      let inputs = ref [ "hello" ;
                         "my" ;
                         "name" ;
                         "is" ;
                         "matt" ;
                         "quit" ; ]
      in function
      | Pure next -> next
      | Join Getline cont ->
         let fake_get_line () =
           match !inputs with
           | [] -> print_endline "Dry run will now exit; \
                                  user never typed \"quit\"." ;
                   exit 0
           | x :: xs ->
              inputs := xs ;
              Printf.printf "(...user is typing \"%s\"...)\n" x ;
              x
         in
         let fake_input = fake_get_line ()
         in fake_input |> cont |> dry_run
      | Join Response (msg, next) ->
         print_endline msg ;
         dry_run next
      | Join Quit -> print_endline "Exiting!" ;
                     Stdlib.exit 0
      | Join Forever action ->
         while true do dry_run action |> ignore done
    let main () = dry_run main
  end

end
