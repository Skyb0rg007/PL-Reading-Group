open Mattlude.Misc

module State = struct
  type state = int
  type 'next t = [
    | `Get of (state -> 'next)
    | `Put of (state * 'next)
    ]

  let map f = function 
    | `Get cont -> `Get (fun x -> cont x |> f)
    | `Put (state, next) -> `Put (state, f next)
end

module StateInterpreter = struct
  module FState = Free.PolyMake (State)
  include FState
  
  let get () = lift (`Get Fun.id)
  let put msg = lift (`Put (msg, ()))

  let modify f =
    let* current = get ()
    in put (f current)
end

module Repl = struct
  type 'next t = [
    | `Getline of (string -> 'next)
    | `Quit
    | `Response of (string * 'next)
    | `Forever of 'next
    ]

  let map f = function
    | `Getline cont -> `Getline (fun x -> cont x |> f)
    | `Response (msg, next) -> `Response (msg, f next)
    | `Quit -> `Quit
    | `Forever action -> `Forever (f action)
end

module ReplInterpreter = struct
  module FRepl = Free.PolyMake (Repl)
  include FRepl

  let getline () = lift (`Getline Fun.id)
  let response msg = lift (`Response (msg, ()))
  let quit () = lift `Quit
  let forever action = `Join (`Forever action)

  let one_round () =
    let* input = getline () in
    let msg =
      Printf.sprintf "You just typed %s!" input
    in
    if input = "quit"
    then quit ()
    else response msg

  let repl () = forever @@ one_round ()
end

module Combined = struct
  type 'next t = [
    | 'next State.t
    | 'next Repl.t
    ]

  let map f = function
    | #State.t as x -> State.map f x
    | #Repl.t as x -> Repl.map f x
end

module CombinedInterpreter = struct
  module FCombinedInterpreter = Free.PolyMake (Combined)
  include FCombinedInterpreter

  let get : unit -> int StateInterpreter.t
      :> unit -> int t = StateInterpreter.get
  let put : int -> unit StateInterpreter.t
      :> int -> unit t = StateInterpreter.put
  let modify : (int -> int) -> unit StateInterpreter.t
      :> (int -> int) -> unit t = StateInterpreter.modify
  let getline : unit -> string ReplInterpreter.t
      :> unit -> string t = ReplInterpreter.getline
  let response : string -> unit ReplInterpreter.t
      :> string -> unit t = ReplInterpreter.response
  let quit : unit -> 'a ReplInterpreter.t
      :> unit -> 'a t = ReplInterpreter.quit
  let forever = ReplInterpreter.forever
  let one_round : unit -> unit ReplInterpreter.t
      :> unit -> unit t = ReplInterpreter.one_round
  let repl : unit -> unit ReplInterpreter.t
      :> unit -> unit t = ReplInterpreter.repl

  let rec run replstate = function
    | `Pure v -> (v, replstate)
    | `Join `Get cont ->
       run replstate (cont replstate)
    | `Join `Put (newstate, next) ->
       run newstate next
    | `Join `Getline cont ->
       let input = read_line ()
       in run replstate (cont input)
    | `Join `Response (msg, next) ->
       print_endline msg ;
       run replstate next
    | `Join `Quit ->
       print_endline "Exiting!" ;
       Stdlib.exit 0
    | `Join `Forever next ->
       let (_, newstate) = run replstate next
       in run newstate (forever next)

  let stateful_prompt =
    let* count1 = get () in
    let* _ = response ("the counter is now "
                       ^ string_of_int count1
                       ^ "!")
    in
    let* _ = one_round () in
    let* _ = modify succ in
    let* count2 = get () in
    response ("the counter is now "
              ^ string_of_int count2
              ^ "!")
end

module Executable = struct
  open CombinedInterpreter
  let main () = fst (run 0 stateful_prompt)
end
