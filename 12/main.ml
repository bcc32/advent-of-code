open! Core
open! Async
open! Import

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

(* let find_cycle x ~equal ~f =
 *   let slow = ref x in
 *   let fast = ref x in
 *   let c = ref 0 in
 *   incr c;
 *   slow := f !slow;
 *   fast := f !fast;
 *   fast := f !fast;
 *   while not (equal slow fast) do
 *     incr c;
 *     slow := f !slow;
 *     fast := f !fast;
 *     fast := f !fast;
 *   done;
 *   { offset = !c; length = 2 * !c }
 * ;; *)

module Moon_one = struct
  type t =
    { mutable x : int
    ; mutable vx : int
    }

  let of_moon (moon : Moon.t) ~which =
    match which with
    | `x -> { x = moon.x; vx = moon.vx }
    | `y -> { x = moon.y; vx = moon.vy }
    | `z -> { x = moon.z; vx = moon.vz }
  ;;
end

let step (moon_ones : Moon_one.t list) =
  List.iter moon_ones ~f:(fun agent ->
    List.iter moon_ones ~f:(fun patient ->
      if not (phys_equal agent patient)
      then patient.vx <- patient.vx + Int.compare agent.x patient.x));
  List.iter moon_ones ~f:(fun m -> m.x <- m.x + m.vx)
;;

let find_cycle_length moon_ones =
  let states =
    Hashtbl.create
      (module struct
        type t = (int * int) list [@@deriving compare, hash, sexp_of]
      end)
  in
  let state_of_moon_ones =
    List.map ~f:(fun (moon_one : Moon_one.t) -> moon_one.x, moon_one.vx)
  in
  let rec loop n =
    step moon_ones;
    match Hashtbl.add states ~key:(state_of_moon_ones moon_ones) ~data:n with
    | `Ok -> loop (n + 1)
    | `Duplicate ->
      (* reached cycle after n + 1 iterations *)
      let last = Hashtbl.find_exn states (state_of_moon_ones moon_ones) in
      let cycle_length = n - last in
      { offset = last; length = cycle_length }
  in
  Hashtbl.add_exn states ~key:(state_of_moon_ones moon_ones) ~data:0;
  loop 1
;;

let b () =
  let%bind moons = input () in
  let xs = List.map moons ~f:(Moon_one.of_moon ~which:`x) in
  let ys = List.map moons ~f:(Moon_one.of_moon ~which:`y) in
  let zs = List.map moons ~f:(Moon_one.of_moon ~which:`z) in
  let x_cycle = find_cycle_length xs in
  let y_cycle = find_cycle_length ys in
  let z_cycle = find_cycle_length zs in
  print_s [%message (x_cycle : cycle) (y_cycle : cycle) (z_cycle : cycle)];
  return ()
;;

(* 290314621566528 is LCM *)
let%expect_test "b" =
  let%bind () = b () in
  [%expect
    {|
    ((x_cycle ((offset 0) (length 84032)))
     (y_cycle ((offset 0) (length 286332)))
     (z_cycle ((offset 0) (length 193052)))) |}]
;;
