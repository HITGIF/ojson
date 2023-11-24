open! Core

let command =
  Command.basic
    ~summary:"Parses Json files/strings and outputs the result"
    (let%map_open.Command str =
       choose_one
         ~if_nothing_chosen:Raise
         [ flag "-file" (optional Filename.arg_type) ~doc:"FILE file to parse"
           |> map ~f:(Option.map ~f:In_channel.read_all)
         ; flag "-string" (optional string) ~doc:"STRING string to parse"
         ]
     in
     fun () -> print_s [%sexp (Ojson.parse str : Ojson.Json.t)])
;;

let () = Command.run command
