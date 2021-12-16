open! Core

type data =
  | Num of int
  | Reg of char

let data_of_string input =
  try Num (Int.of_string input) with
  | _ -> Reg input.[0]
;;

type command =
  | Set of char * data
  | Sub of char * data
  | Mul of char * data
  | Jnz of data * data

let command_of_string input =
  match String.split input ~on:' ' with
  | [ "set"; a; b ] -> Set (a.[0], data_of_string b)
  | [ "sub"; a; b ] -> Sub (a.[0], data_of_string b)
  | [ "mul"; a; b ] -> Mul (a.[0], data_of_string b)
  | [ "jnz"; a; b ] -> Jnz (data_of_string a, data_of_string b)
  | _ -> failwith ("invalid command: " ^ input)
;;

let num_mul = ref 0
let state = Char.Table.create ()

let run input =
  let get_value data =
    match data with
    | Num n -> n
    | Reg x -> Hashtbl.find_exn state x
  in
  let rec loop pc =
    if not (pc >= 0 && pc < Array.length input)
    then ()
    else (
      let command = input.(pc) in
      let get_or_zero x = Option.value x ~default:0 in
      match command with
      | Set (r, y) ->
        Hashtbl.set state ~key:r ~data:(get_value y);
        loop (pc + 1)
      | Sub (r, y) ->
        Hashtbl.update state r ~f:(fun x -> get_or_zero x - get_value y);
        loop (pc + 1)
      | Mul (r, y) ->
        incr num_mul;
        Hashtbl.update state r ~f:(fun x -> get_or_zero x * get_value y);
        loop (pc + 1)
      | Jnz (x, y) -> if get_value x <> 0 then loop (pc + get_value y) else loop (pc + 1))
  in
  loop 0
;;

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.map ~f:command_of_string
    |> List.to_array
  in
  for i = 0 to 7 do
    Hashtbl.set state ~key:(Char.of_int_exn (Char.to_int 'a' + i)) ~data:0
  done;
  run input;
  printf "%d\n" !num_mul
;;
