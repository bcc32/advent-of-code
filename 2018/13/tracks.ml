open! Core
open! Async
open! Import

module Track = struct
  type t = char
end

type t = Track.t option array array

let read () =
  Reader.file_lines "input"
  >>| Array.of_list_map ~f:(fun row ->
    String.to_array row
    |> Array.map ~f:(function
      | ('\\' | '/' | '|' | '-' | '+') as c -> Some c
      | '>' | '<' -> Some '-'
      | '^' | 'v' -> Some '|'
      | _ -> None))
;;

let get_track_exn t ((row, col) as pos) =
  Option.value_exn
    t.(row).(col)
    ~error:(Error.create "no track at position" pos [%sexp_of: int * int])
;;
