open! Core
open! Async
open! Import
open Intcode

let input () = Reader.file_contents "input" >>| Program.of_string

let main ~init ~extra_input =
  let%bind program = input () in
  match Program.Async.run program with
  | { input; output; done_ } ->
    let%bind () =
      let%bind () =
        Pipe.transfer_in
          input
          ~from:(Queue.of_list (String.to_list init |> List.map ~f:Char.to_int))
      in
      Pipe.transfer' extra_input input ~f:(fun sq ->
        return
          (Queue.concat_map sq ~f:(fun s -> String.to_list s |> List.map ~f:Char.to_int)))
    and () =
      Pipe.iter_without_pushback output ~f:(fun c -> print_char (Char.of_int_exn c))
    and () = done_ in
    return ()
;;

let command =
  Command.async
    ~summary:"Play"
    (Command.Param.return (fun () ->
       let%bind init = Reader.file_contents "init.txt" in
       let extra_input = force Reader.stdin |> Reader.pipe in
       main ~init ~extra_input))
;;

let a () =
  let init =
    [ "east"
    ; "south"
    ; "south"
    ; "take hologram"
    ; "north"
    ; "north"
    ; "west"
    ; "south"
    ; "take mouse"
    ; "east"
    ; "take shell"
    ; "west"
    ; "west"
    ; "take whirled peas"
    ; "east"
    ; "north"
    ; "west"
    ; "north"
    ; "west"
    ; "south"
    ; "take hypercube"
    ; "north"
    ; "east"
    ; "north"
    ; "west"
    ; "take semiconductor"
    ; "east"
    ; "south"
    ; "south"
    ; "west"
    ; "take antenna"
    ; "south"
    ; "take spool of cat6"
    ; "north"
    ; "west"
    ; "south"
    ; "south"
    ; "drop hologram"
    ; "drop shell"
    ; "drop spool of cat6"
    ; "drop whirled peas"
    ; "south"
    ]
    |> List.map ~f:(fun s -> s ^ "\n")
    |> String.concat
  in
  (* FIXME: Use Pipe.empty when available. *)
  let%bind () = main ~init ~extra_input:(Pipe.of_list []) in
  return ()
;;

let%expect_test "a" =
  let%bind () = a () in
  [%expect
    {|
    == Hull Breach ==
    You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed.

    Doors here lead:
    - east
    - south
    - west

    Command?



    == Stables ==
    Reindeer-sized. They're all empty.

    Doors here lead:
    - south
    - west

    Command?



    == Storage ==
    The boxes just contain more boxes.  Recursively.

    Doors here lead:
    - north
    - south

    Items here:
    - giant electromagnet

    Command?



    == Arcade ==
    None of the cabinets seem to have power.

    Doors here lead:
    - north

    Items here:
    - hologram

    Command?

    You take the hologram.

    Command?



    == Storage ==
    The boxes just contain more boxes.  Recursively.

    Doors here lead:
    - north
    - south

    Items here:
    - giant electromagnet

    Command?



    == Stables ==
    Reindeer-sized. They're all empty.

    Doors here lead:
    - south
    - west

    Command?



    == Hull Breach ==
    You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed.

    Doors here lead:
    - east
    - south
    - west

    Command?



    == Observatory ==
    There are a few telescopes; they're all bolted down, though.

    Doors here lead:
    - north
    - east
    - west

    Items here:
    - mouse

    Command?

    You take the mouse.

    Command?



    == Engineering ==
    You see a whiteboard with plans for Springdroid v2.

    Doors here lead:
    - west

    Items here:
    - shell

    Command?

    You take the shell.

    Command?



    == Observatory ==
    There are a few telescopes; they're all bolted down, though.

    Doors here lead:
    - north
    - east
    - west

    Command?



    == Sick Bay ==
    Supports both Red-Nosed Reindeer medicine and regular reindeer medicine.

    Doors here lead:
    - east

    Items here:
    - whirled peas

    Command?

    You take the whirled peas.

    Command?



    == Observatory ==
    There are a few telescopes; they're all bolted down, though.

    Doors here lead:
    - north
    - east
    - west

    Command?



    == Hull Breach ==
    You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed.

    Doors here lead:
    - east
    - south
    - west

    Command?



    == Science Lab ==
    You see evidence here of prototype polymer design work.

    Doors here lead:
    - north
    - east
    - west

    Command?



    == Hot Chocolate Fountain ==
    Somehow, it's still working.

    Doors here lead:
    - north
    - south
    - west

    Items here:
    - molten lava

    Command?



    == Passages ==
    They're a little twisty and starting to look all alike.

    Doors here lead:
    - north
    - east
    - south

    Command?



    == Crew Quarters ==
    The beds are all too small for you.

    Doors here lead:
    - north

    Items here:
    - hypercube

    Command?

    You take the hypercube.

    Command?



    == Passages ==
    They're a little twisty and starting to look all alike.

    Doors here lead:
    - north
    - east
    - south

    Command?



    == Hot Chocolate Fountain ==
    Somehow, it's still working.

    Doors here lead:
    - north
    - south
    - west

    Items here:
    - molten lava

    Command?



    == Gift Wrapping Center ==
    How else do you wrap presents on the go?

    Doors here lead:
    - south
    - west

    Items here:
    - infinite loop

    Command?



    == Hallway ==
    This area has been optimized for something; you're just not quite sure what.

    Doors here lead:
    - east

    Items here:
    - semiconductor

    Command?

    You take the semiconductor.

    Command?



    == Gift Wrapping Center ==
    How else do you wrap presents on the go?

    Doors here lead:
    - south
    - west

    Items here:
    - infinite loop

    Command?



    == Hot Chocolate Fountain ==
    Somehow, it's still working.

    Doors here lead:
    - north
    - south
    - west

    Items here:
    - molten lava

    Command?



    == Science Lab ==
    You see evidence here of prototype polymer design work.

    Doors here lead:
    - north
    - east
    - west

    Command?



    == Holodeck ==
    Someone seems to have left it on the Giant Grid setting.

    Doors here lead:
    - east
    - south
    - west

    Items here:
    - antenna

    Command?

    You take the antenna.

    Command?



    == Kitchen ==
    Everything's freeze-dried.

    Doors here lead:
    - north

    Items here:
    - spool of cat6

    Command?

    You take the spool of cat6.

    Command?



    == Holodeck ==
    Someone seems to have left it on the Giant Grid setting.

    Doors here lead:
    - east
    - south
    - west

    Command?



    == Warp Drive Maintenance ==
    It appears to be working normally.

    Doors here lead:
    - east
    - south

    Items here:
    - photons

    Command?



    == Navigation ==
    Status: Stranded. Please supply measurements from fifty stars to recalibrate.

    Doors here lead:
    - north
    - south

    Items here:
    - escape pod

    Command?



    == Security Checkpoint ==
    In the next room, a pressure-sensitive floor will verify your identity.

    Doors here lead:
    - north
    - south

    Command?

    You drop the hologram.

    Command?

    You drop the shell.

    Command?

    You drop the spool of cat6.

    Command?

    You drop the whirled peas.

    Command?



    == Pressure-Sensitive Floor ==
    Analyzing...

    Doors here lead:
    - north

    A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
    Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
    "Oh, hello! You should be able to get in by typing 20483 on the keypad at the main airlock." |}]
;;
