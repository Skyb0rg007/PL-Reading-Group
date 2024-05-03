
module E = Ocaml_errors.Cps_error

type app_error = ApplicationError of string
type conf_error = ConfigError of string
type app_trace = ApplicationTrace
type conf_trace = ConfigTrace

module AppError = E.MakeError(struct
    type t = app_error

    let to_string (ApplicationError msg) = "ApplicationError " ^ msg
end)

module ConfError = E.MakeError(struct
    type t = conf_error

    let to_string (ConfigError msg) = "ConfigError " ^ msg
end)

module AppTrace = E.MakeTrace(struct
    type t = app_trace

    let to_string ApplicationTrace = "ApplicationTrace"
end)

module ConfTrace = E.MakeTrace(struct
    type t = conf_trace

    let to_string ConfigTrace = "ConfigTrace"
end)

let configure () =
    let open E in
    ConfTrace.with_context ConfigTrace @@
    AppError.throw (ApplicationError "Error!")

let r = E.run (configure ())

let () =
    match r with
    | Result.Ok () -> print_endline "Ok"
    | Result.Error (trace, err) ->
            print_endline "Error";
            print_endline ("Trace: [" ^ String.concat "," (List.map E.string_of_trace trace) ^ "]");
            print_endline ("Error: " ^ E.string_of_error err)
    
