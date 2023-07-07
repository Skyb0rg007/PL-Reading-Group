open Effect
open Effect.Deep

open Mattlude.Endofunctors

(* I stole the monadic reflection code from
   https://github.com/ocaml-multicore/effects-examples *)
module RR (M : MONAD) :
sig
  val reify : (unit -> 'a) -> 'a M.t
  val reflect : 'a M.t -> 'a
end =
struct
  type _ Effect.t += E : 'a M.t -> 'a Effect.t
  let reify f = match_with f () {
    retc = (fun x -> M.pure x);
    exnc = raise;
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | E m -> Some (fun k -> M.bind m (continue k))
      | _ -> None
  }
  let reflect m = perform (E m)
end

module StateLogic =
struct
  type 'a t = int -> int * 'a
  let pure v s = (s, v)
  let bind m k s = let s, a = m s in k a s
  let get s = (s, s)
  let put s _ = (s, ())
  let run s ~init = s init
end

(* Reify and reflect for StateLogic *)
module StateR = struct
  module SR = RR(StateLogic)
  let put v = SR.reflect (StateLogic.put v)
  let get () = SR.reflect StateLogic.get
  let run_state f ~init =
    let _, v = StateLogic.run (SR.reify f) ~init in
    v
end

module State = struct
  type state = int
  type _ Effect.t += Get : state t
  type _ Effect.t += Put : state -> unit t
end

module StateInterpreter = struct
  include State

  let get () = perform Get
  let put state = perform (Put state)

  let modify f =
    let current = get ()
    in put (f current)
end

module Repl = struct
  type _ Effect.t += Getline : string t
  type _ Effect.t += Quit : unit t
  type _ Effect.t += Response : string -> unit t
  type _ Effect.t += Forever : (unit -> unit) -> unit t
end

module ReplInterpreter = struct
  include Repl

  let getline () = perform Getline
  let quit () = perform Quit
  let response input = perform (Response input)
  let forever f = perform (Forever f)

  let one_round () =
    let input = getline () in
    let msg =
      Printf.sprintf "You just typed %s!" input
    in
    if input = "quit"
    then quit ()
    else response msg

  let repl () = forever one_round
end

module CombinedInterpreter = struct
  include StateInterpreter
  include ReplInterpreter

  let rec interpret repl =
    try_with repl ()
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
                      continue k (while true do interpret f |> ignore done))
               | Get ->
                  Some (fun (k : (a, _) continuation) ->
                      continue k (StateR.get ()))
               | Put newstate ->
                  Some (fun (k : (a, _) continuation) ->
                      continue k (StateR.put newstate))
               | _ -> None }

  let stateful_prompt () =
    let count1 = get () in
    let () = response ("the counter is now "
                       ^ string_of_int count1
                       ^ "!")
    in
    let () = one_round () in
    let () = modify succ in
    let count2 = get () in
    response ("the counter is now "
              ^ string_of_int count2
              ^ "!")
end

module Executable = struct
  open CombinedInterpreter
  let main () =
    StateR.run_state ~init:0 (fun () -> interpret stateful_prompt)
end
