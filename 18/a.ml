open! Core

type data =
  | Num of int
  | Reg of char

let data_of_string input =
  try Num (Int.of_string input) with
  | _ -> Reg input.[0]
;;

type command =
  | Snd of data
  | Rcv of data
  | Set of char * data
  | Add of char * data
  | Mul of char * data
  | Mod of char * data
  | Jgz of data * data

let command_of_string input =
  match String.split input ~on:' ' with
  | [ "snd"; a ] -> Snd (data_of_string a)
  | [ "rcv"; a ] -> Rcv (data_of_string a)
  | [ "set"; a; b ] -> Set (a.[0], data_of_string b)
  | [ "add"; a; b ] -> Add (a.[0], data_of_string b)
  | [ "mul"; a; b ] -> Mul (a.[0], data_of_string b)
  | [ "mod"; a; b ] -> Mod (a.[0], data_of_string b)
  | [ "jgz"; a; b ] -> Jgz (data_of_string a, data_of_string b)
  | _ -> failwith ("invalid command: " ^ input)
;;

let state = Char.Table.create ()

let get_value data =
  match data with
  | Num n -> n
  | Reg x -> Hashtbl.find_exn state x
;;

let () =
  let input =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:In_channel.input_lines
    |> List.map ~f:command_of_string
    |> List.to_array
  in
  let last_sound = ref None in
  with_return (fun { return } ->
    let rec loop pc =
      if not (pc >= 0 && pc < Array.length input)
      then ()
      else (
        let command = input.(pc) in
        let get_or_zero x = Option.value x ~default:0 in
        match command with
        | Snd x -> last_sound := Some (get_value x); loop (pc + 1)
        | Rcv x ->
          if get_value x <> 0
          then (return (uw !last_sound));
          loop (pc + 1)
        | Set (r, y) -> Hashtbl.set state ~key:r ~data:(get_value y); loop (pc + 1)
        | Add (r, y) -> Hashtbl.update state r ~f:(fun x -> get_or_zero x + get_value y); loop (pc + 1)
        | Mul (r, y) -> Hashtbl.update state r ~f:(fun x -> get_or_zero x * get_value y); loop (pc + 1)
        | Mod (r, y) -> Hashtbl.update state r ~f:(fun x -> get_or_zero x % get_value y); loop (pc + 1)
        | Jgz (x, y) ->
          if get_value x > 0
          then (loop (pc + get_value y))
          else (loop (pc + 1)))
    in
    loop 0;
    assert false)
  |> printf "%d\n"
;;
