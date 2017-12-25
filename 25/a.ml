open! Core

type step =
  { write : bool
  ; move : [ `Left | `Right ]
  ; next : string }

type state =
  { if_zero : step
  ; if_one  : step }

let tape = Int.Table.create ()

let read_tape i = Hashtbl.find_or_add tape i ~default:(fun () -> false)

let states = String.Table.create ()

let run_step state pos =
  let step =
    let state = Hashtbl.find_exn states !state in
    match read_tape !pos with
    | false -> state.if_zero
    | true -> state.if_one
  in
  let { write; move; next } = step in
  Hashtbl.set tape ~key:!pos ~data:write;
  (match move with
   | `Left -> decr pos
   | `Right -> incr pos);
  state := next
;;

let () =
  let (start, steps) =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      let line () = In_channel.input_line_exn file in
      let start =
        line ()
        |> String.subo ~len:1 ~pos:15
      in
      let steps =
        line ()
        |> String.subo ~pos:36
        |> String.split ~on:' '
        |> List.hd_exn
        |> Int.of_string
      in
      let read_step () =
        ignore (line () : string);
        let value = (line ()).[22] = '1' in
        let move = if (line ()).[27] = 'r' then `Right else `Left in
        let next = String.sub (line ()) ~pos:26 ~len:1 in
        { write = value; move; next }
      in
      let rec loop () =
        match line () with
        | exception End_of_file -> ()
        | _ ->
          let s = line () in
          let name = String.subo s ~pos:9 ~len:1 in
          let if_zero = read_step () in
          let if_one  = read_step () in
          let state = { if_zero; if_one } in
          Hashtbl.add_exn states ~key:name ~data:state;
          loop ()
      in
      loop ();
      (start, steps))
  in
  let state = ref start in
  let pos = ref 0 in
  for _ = 1 to steps do
    run_step state pos
  done;
  Hashtbl.count tape ~f:Fn.id
  |> printf "%d\n"
;;
