open! Core
open! Async
include Expect_test_helpers_core
include Expect_test_helpers_async
include Advent_of_code_lattice_geometry

let ( << ) f g x = f (g x)
let ( >> ) f g x = x |> f |> g
