open! Core
open! Async
include Expect_test_helpers_core
include Expect_test_helpers_async

let ( << ) f g x = f (g x)
let ( >> ) f g x = x |> f |> g
