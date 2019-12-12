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
  let%map lines = Reader.file_lines "input" in
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
  [%expect {| 12644 |}]
;;

type cycle =
  { offset : int
  ; length : int
  }
[@@deriving sexp_of]

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
  moon_ones
  |> List.map ~f:(fun moon_one ->
    { moon_one with
      vx =
        moon_one.vx
        + List.sum
            (module Int)
            moon_ones
            ~f:(fun other ->
              if not (phys_equal moon_one other)
              then Int.compare other.x moon_one.x
              else 0)
    })
  |> List.map ~f:(fun moon_one -> { moon_one with x = moon_one.x + moon_one.vx })
;;

(* TODO: Extract this into ProjectEuler solutions. *)
let find_cycle_length moon_ones =
  let rec loop power cycle_length slow fast =
    if [%equal: Moon_one.t list] slow fast
    then cycle_length
    else if power = cycle_length
    then loop (power * 2) 1 fast (step fast)
    else loop power (cycle_length + 1) slow (step fast)
  in
  let cycle_length = loop 1 1 moon_ones (step moon_ones) in
  let rec loop offset slow fast =
    if [%equal: Moon_one.t list] slow fast
    then offset
    else loop (offset + 1) (step slow) (step fast)
  in
  let offset = loop 0 moon_ones (Fn.apply_n_times step moon_ones ~n:cycle_length) in
  { offset; length = cycle_length }
;;

let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a / gcd a b * b

let b () =
  let%bind moons = input () in
  let xs = List.map moons ~f:(Moon_one.of_moon ~which:`x) in
  let ys = List.map moons ~f:(Moon_one.of_moon ~which:`y) in
  let zs = List.map moons ~f:(Moon_one.of_moon ~which:`z) in
  let x_cycle = find_cycle_length xs in
  let y_cycle = find_cycle_length ys in
  let z_cycle = find_cycle_length zs in
  if debug then print_s [%message (x_cycle : cycle) (y_cycle : cycle) (z_cycle : cycle)];
  (* Simplifying assumptions. *)
  assert (x_cycle.offset = 0);
  assert (y_cycle.offset = 0);
  assert (z_cycle.offset = 0);
  List.reduce_exn [ x_cycle.length; y_cycle.length; z_cycle.length ] ~f:lcm
  |> printf "%d\n";
  return ()
;;

let%expect_test "b" =
  let%bind () = b () in
  [%expect {| 290314621566528 |}]
;;
