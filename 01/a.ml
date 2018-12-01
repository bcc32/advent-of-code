open! Core
open! Async

let main () =
  Reader.with_file "input" ~f:(fun r ->
    Reader.lines r
    |> Pipe.fold_without_pushback ~init:0 ~f:(fun acc line -> acc + Int.of_string line))
  >>| printf "%d\n"
;;

let command =
  Command.async
    ~summary:"01-a"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> main ())
;;

let () = Command.run command
