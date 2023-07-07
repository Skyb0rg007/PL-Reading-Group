open Effect
open Effect.Deep

type _ Effect.t += Getline : string t
type _ Effect.t += Quit : unit t
type _ Effect.t += Response : string -> unit t
type _ Effect.t += Forever : (unit -> unit) -> unit t

let get_line () = perform Getline
let quit () = perform Quit
let response input = perform (Response input)
let forever f = perform (Forever f)

let main () =
  let one_round () =
    let input = get_line () in
    let msg =
      Printf.sprintf "You just typed %s!" input
    in
    if input = "quit"
    then quit ()
    else response msg
  in
  forever one_round

module Executable = struct
  module Real = struct
    let rec real_repl main =
      try_with main () 
        { effc = fun (type a) (eff : a t) ->
                 match eff with
                 | Getline ->
                    Some (fun (k : (a, _) continuation) ->
                        continue k (read_line ()))
                 | Quit -> print_endline "Exiting!" ;
                           Stdlib.exit 0
                 | Response msg ->
                    Some (fun (k : (a, _) continuation) ->
                        continue k (print_endline msg))
                 | Forever f ->
                    Some (fun (k : (a, _) continuation) ->
                        continue k (while true do real_repl f done))
                 | _ -> None }
    let main () = real_repl main
  end

  module Dry = struct
    let dry_run main =
      let inputs = ref [ "hello" ;
                         "my";
                         "name" ;
                         "is" ;
                         "matt";
                         "quit" ; ]
      in
      let rec dry_run' main =
        try_with main ()
          { effc = fun (type a) (eff: a t) ->
                   match eff with
                   | Getline ->
                      Some (fun (k: (a, _) continuation) ->
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
                          continue k (fake_get_line ()))
                   | Quit -> print_endline "Exiting!" ;
                             Stdlib.exit 0
                   | Response msg ->
                      Some (fun (k: (a, _) continuation) ->
                          continue k (print_endline msg))
                   | Forever f ->
                      Some (fun (k: (a, _) continuation) ->
                          continue k (while true do dry_run' f done))
                   | _ -> None }
      in dry_run' main
    let main () = dry_run main
  end
end
