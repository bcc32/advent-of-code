open! Core
open! Async
open! Import

module Opcode = struct
  type t =
    | Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
  [@@deriving enumerate]
end

type t =
  { opcode : int
  ; a : int
  ; b : int
  ; c : int
  }

let of_string s =
  match s |> String.split ~on:' ' |> List.map ~f:Int.of_string with
  | [ opcode; a; b; c ] -> { opcode; a; b; c }
  | _ -> raise_s [%message "Insn.of_string: wrong number of fields" ~_:(s : string)]
;;

let exec { opcode; a; b; c } ~memory ~op =
  match (op opcode : Opcode.t) with
  | Addr -> memory.(c) <- memory.(a) + memory.(b)
  | Addi -> memory.(c) <- memory.(a) + b
  | Mulr -> memory.(c) <- memory.(a) * memory.(b)
  | Muli -> memory.(c) <- memory.(a) * b
  | Banr -> memory.(c) <- memory.(a) land memory.(b)
  | Bani -> memory.(c) <- memory.(a) land b
  | Borr -> memory.(c) <- memory.(a) lor memory.(b)
  | Bori -> memory.(c) <- memory.(a) lor b
  | Setr -> memory.(c) <- memory.(a)
  | Seti -> memory.(c) <- a
  | Gtir -> memory.(c) <- Bool.to_int (a > memory.(b))
  | Gtri -> memory.(c) <- Bool.to_int (memory.(a) > b)
  | Gtrr -> memory.(c) <- Bool.to_int (memory.(a) > memory.(b))
  | Eqir -> memory.(c) <- Bool.to_int (a = memory.(b))
  | Eqri -> memory.(c) <- Bool.to_int (memory.(a) = b)
  | Eqrr -> memory.(c) <- Bool.to_int (memory.(a) = memory.(b))
;;
