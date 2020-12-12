open! Core
open! Import

type 'dir_type t =
  { position : Vec2.t
  ; dir : 'dir_type Dir.t
  }
[@@deriving sexp_of]
