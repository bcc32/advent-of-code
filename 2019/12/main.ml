open! Core
open! Async
open! Import

let debug = false

module Moon = struct
  type t =
    { mutable x : int
    ; mutable y : int
    ; mutable z : int
    ; mutable vx : int
    ; mutable vy : int
    ; mutable vz : int
    }

  let energy { x; y; z; vx; vy; vz } =
    List.sum (module Int) [ x; y; z ] ~f:Int.abs
    * List.sum (module Int) [ vx; vy; vz ] ~f:Int.abs
  ;;

  let of_string =
    let rex =
      let open Re in
      compile
        (seq
           [ str "<x="
           ; group (seq [ opt (char '-'); rep1 digit ])
           ; str ", y="
           ; group (seq [ opt (char '-'); rep1 digit ])
           ; str ", z="
           ; group (seq [ opt (char '-'); rep1 digit ])
           ; str ">"
           ])
    in
    fun s ->
      let g = Re.exec rex s in
      let x = Int.of_string (Re.Group.get g 1) in
      let y = Int.of_string (Re.Group.get g 2) in
      let z = Int.of_string (Re.Group.get g 3) in
      { x; y; z; vx = 0; vy = 0; vz = 0 }
  ;;
end

let input () =
  let%map lines = Reader.file_lines "aoc.in" in
  List.map lines ~f:Moon.of_string
;;

let step (moons : Moon.t list) =
  List.iter moons ~f:(fun agent ->
    List.iter moons ~f:(fun patient ->
      if not (phys_equal agent patient)
      then (
        patient.vx <- patient.vx + Int.compare agent.x patient.x;
        patient.vy <- patient.vy + Int.compare agent.y patient.y;
        patient.vz <- patient.vz + Int.compare agent.z patient.z)));
  List.iter moons ~f:(fun m ->
    m.x <- m.x + m.vx;
    m.y <- m.y + m.vy;
    m.z <- m.z + m.vz)
;;

let a () =
  let%bind moons = input () in
  for _ = 1 to 1000 do
    step moons
  done;
  List.sum (module Int) moons ~f:Moon.energy |> printf "%d\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect {| 12644 |}];
  return ()
;;

module Cycle = struct
  type t =
    { offset : int
    ; length : int
    }
  [@@deriving sexp_of]

  let find ~start ~f ~equal =
    let length, offset = Euler.Sequences.find_cycle ~start ~f ~equal in
    { offset; length }
  ;;
end

(* Simulate each vector component of the moons separately, then combine using
   least common multiple. *)
module Moon_one = struct
  type t =
    { x : int
    ; vx : int
    }
  [@@deriving equal, sexp_of]

  let of_moon (moon : Moon.t) ~which =
    match which with
    | `x -> { x = moon.x; vx = moon.vx }
    | `y -> { x = moon.y; vx = moon.vy }
    | `z -> { x = moon.z; vx = moon.vz }
  ;;
end

let step (moon_ones : Moon_one.t list) =
  let gravity one ~on =
    if phys_equal on one then 0 else Int.compare one.Moon_one.x on.Moon_one.x
  in
  moon_ones
  |> List.map ~f:(fun moon_one ->
    { moon_one with
      vx = moon_one.vx + List.sum (module Int) moon_ones ~f:(gravity ~on:moon_one)
    })
  |> List.map ~f:(fun moon_one -> { moon_one with x = moon_one.x + moon_one.vx })
;;

let b () =
  let%bind moons = input () in
  let xs = List.map moons ~f:(Moon_one.of_moon ~which:`x) in
  let ys = List.map moons ~f:(Moon_one.of_moon ~which:`y) in
  let zs = List.map moons ~f:(Moon_one.of_moon ~which:`z) in
  let x_cycle = Cycle.find ~start:xs ~f:step ~equal:[%equal: Moon_one.t list] in
  let y_cycle = Cycle.find ~start:ys ~f:step ~equal:[%equal: Moon_one.t list] in
  let z_cycle = Cycle.find ~start:zs ~f:step ~equal:[%equal: Moon_one.t list] in
  if debug
  then print_s [%message (x_cycle : Cycle.t) (y_cycle : Cycle.t) (z_cycle : Cycle.t)];
  (* Simplifying assumptions.  If the offsets were not zero, we could use, e.g.,
     the Chinese Remainder Theorem to calculate the required modulus and
     residue. *)
  assert (x_cycle.offset = 0);
  assert (y_cycle.offset = 0);
  assert (z_cycle.offset = 0);
  List.reduce_exn
    [ x_cycle.length; y_cycle.length; z_cycle.length ]
    ~f:Euler.Number_theory.Int.lcm
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 290314621566528 |}];
  return ()
;;
