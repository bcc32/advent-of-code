open! Core
open! Async

let ( << ) f g x = f (g x)
let ( >> ) f g x = x |> f |> g
