open! Core
open! Async
open! Import

type ev =
  | Begin_shift of int
  | Sleep
  | Wake
[@@deriving sexp]

type t =
  { event : ev
  ; minute : int
  }
[@@deriving sexp]

let event_re, begin_shift_mark, sleep_mark, wake_mark =
  let begin_shift_mark, begin_shift =
    Re.(mark (seq [ str "Guard #"; group (rep1 digit); str " begins shift" ]))
  in
  let sleep_mark, sleep = Re.(mark (str "falls asleep")) in
  let wake_mark, wake = Re.(mark (str "wakes up")) in
  ( Re.(
      compile
        (seq
           [ char '['
           ; rep1 (compl [ char ':' ])
           ; char ':'
           ; group (rep1 digit)
           ; str "] "
           ; alt [ begin_shift; sleep; wake ]
           ]))
  , begin_shift_mark
  , sleep_mark
  , wake_mark )
;;

let of_string line =
  let group = Re.exec event_re line in
  let minute = Re.Group.get group 1 |> Int.of_string in
  let event =
    if Re.Mark.test group begin_shift_mark
    then (
      let guard_id = Re.Group.get group 2 |> Int.of_string in
      Begin_shift guard_id)
    else if Re.Mark.test group sleep_mark
    then Sleep
    else if Re.Mark.test group wake_mark
    then Wake
    else assert false
  in
  { event; minute }
;;

(* must be in chronological order *)
let analyze events =
  let rec begin_shift events shifts =
    match events with
    | { event = Begin_shift guard_id; minute = _ } :: events ->
      perform_shift events shifts ~guard_id
    | _ :: _ -> failwith "could not start shift"
    | [] -> List.rev shifts
  and perform_shift events shifts ~guard_id =
    match events with
    | { event = Sleep; minute = sleep_minute }
      :: { event = Wake; minute = wake_minute } :: events ->
      let minutes_asleep = sleep_minute, wake_minute in
      perform_shift events ((guard_id, minutes_asleep) :: shifts) ~guard_id
    | { event = Begin_shift _; minute = _ } :: _ as events -> begin_shift events shifts
    | [] -> begin_shift events shifts
    | _ -> raise_s [%message "invalid shift" (events : t list)]
  in
  begin_shift events []
;;
