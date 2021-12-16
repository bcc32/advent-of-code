open! Core

type event = Garbage

let parse input ~f =
  let index = ref 0 in
  let peek () = if !index < String.length input then Some input.[!index] else None in
  let advance () = incr index in
  let rec item () =
    match peek () with
    | None -> raise_s [%message "unexpected EOF"]
    | Some '!' ->
      advance ();
      advance ();
      item ()
    | Some '{' ->
      advance ();
      zero_or_more_items ();
      assert ([%equal: char option] (peek ()) (Some '}'));
      advance ()
    | Some '<' ->
      advance ();
      discard ()
    | Some c -> raise_s [%message "unexpected" (c : char)]
  and zero_or_more_items () =
    match peek () with
    | None -> raise_s [%message "unexpected EOF"]
    | Some '!' ->
      advance ();
      advance ();
      zero_or_more_items ()
    | Some '{' | Some '<' ->
      item ();
      items_tail ()
    | Some '}' -> ()
    | Some c -> raise_s [%message "unexpected" (c : char)]
  and items_tail () =
    match peek () with
    | None -> raise_s [%message "unexpected EOF"]
    | Some '!' ->
      advance ();
      advance ();
      items_tail ()
    | Some ',' ->
      advance ();
      zero_or_more_items ()
    | Some '}' -> ()
    | Some c -> raise_s [%message "unexpected" (c : char)]
  and discard () =
    match peek () with
    | None -> raise_s [%message "unexpected EOF"]
    | Some '!' ->
      advance ();
      advance ();
      discard ()
    | Some '>' -> advance ()
    | Some _ ->
      f Garbage;
      advance ();
      discard ()
  in
  item ()
;;

let () =
  let garbage = ref 0 in
  let f Garbage = incr garbage in
  In_channel.with_file
    (Sys.get_argv ()).(1)
    ~f:(fun file -> In_channel.input_all file |> String.strip)
  |> parse ~f;
  printf "%d\n" !garbage
;;
