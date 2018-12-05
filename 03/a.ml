open! Core
open! Async
open! Import

module Claim = struct
  type t =
    { x : int
    ; y : int
    ; w : int
    ; h : int
    }

  let claim_re =
    Re.(
      compile
        (seq
           [ char '#'
           ; rep1 digit
           ; str " @ "
           ; group (rep1 digit)
           ; char ','
           ; group (rep1 digit)
           ; str ": "
           ; group (rep1 digit)
           ; char 'x'
           ; group (rep1 digit)
           ]))
  ;;

  let of_string line =
    let groups = Re.exec claim_re line in
    let get n = Re.Group.get groups n |> Int.of_string in
    { x = get 1; y = get 2; w = get 3; h = get 4 }
  ;;

  let iter t ~f =
    for x = t.x to t.x + t.w - 1 do
      for y = t.y to t.y + t.h - 1 do
        f x y
      done
    done
  ;;
end

let main () =
  let%bind claims =
    Reader.with_file "input" ~f:(fun r ->
      r |> Reader.lines |> Pipe.map ~f:Claim.of_string |> Pipe.to_list)
  in
  let claims_by_square = Hashtbl.create (module Tuple.Hashable_t (Int) (Int)) in
  List.iter claims ~f:(Claim.iter ~f:(fun x y -> Hashtbl.incr claims_by_square (x, y)));
  claims_by_square |> Hashtbl.count ~f:(fun x -> x >= 2) |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| 109785 |}]
;;
