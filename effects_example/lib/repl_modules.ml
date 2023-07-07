module type REPL = sig
  val get_line : unit -> string
  val quit : unit -> unit
  val response : string -> unit
  val forever : (unit -> unit) -> unit
end

module Repl (R : REPL) = struct
  let main () =
    let one_round () =
      let input = R.get_line () in
      let msg =
        Printf.sprintf "You just typed %s!" input
      in
      if input = "quit"
      then R.quit ()
      else R.response msg
    in
    R.forever one_round
end

module Executable = struct
  module AllRepls = struct
    let quit _ = print_endline "Exiting!" ;
                 Stdlib.exit 0
    let forever f = while true do f () done
    let response input = print_endline input
  end

  module Dry = struct
    module DryRunStruct = struct
      include AllRepls
      let inputs = ref [ "hello" ;
                         "my" ;
                         "name" ;
                         "is" ;
                         "matt" ;
                         "quit" ; ]
      let get_line () =
        match !inputs with
        | [] -> print_endline "Dry run will now exit; \
                               user never typed \"quit\"." ;
                exit 0
        | x :: xs ->
           inputs := xs ;
           Printf.printf "(...user is typing \"%s\"...)\n" x ;
           x
    end

    module _ : REPL = DryRunStruct

    module DryRun = Repl (DryRunStruct)
    let main = DryRun.main
  end

  module Real = struct
    module RealReplStruct = struct
      include AllRepls
      let get_line = Stdlib.read_line
    end

    module _ : REPL = RealReplStruct
    
    module RealRepl = Repl (RealReplStruct)
    let main = RealRepl.main
  end
end
