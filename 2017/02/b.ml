open! Core

let get_whole_division nums =
  List.find_map nums ~f:(fun a ->
    List.find_map nums ~f:(fun b -> if a <> b && a % b = 0 then Some (a / b) else None))
;;

let () =
  In_channel.with_file
    (Sys.get_argv ()).(1)
    ~f:(fun file ->
      In_channel.input_lines file
      |> List.sum
           (module Int)
           ~f:(fun row ->
             let nums = String.split row ~on:'\t' |> List.map ~f:Int.of_string in
             get_whole_division nums |> Option.value_exn)
      |> printf "%d\n")
;;
