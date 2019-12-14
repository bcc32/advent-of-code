open! Core

let redistribute blocks =
  let index =
    Sequence.zip (Sequence.range 0 (List.length blocks)) (Sequence.of_list blocks)
    |> Sequence.min_elt ~compare:(fun (i, x) (j, y) ->
      match Int.descending x y with
      | 0 -> Int.ascending i j
      | n -> n)
    |> uw
    |> fst
  in
  let blocks = Array.of_list blocks in
  let extra = blocks.( index ) in
  blocks.( index ) <- 0;
  let residue = ref (extra % Array.length blocks) in
  for i = 1 to Array.length blocks do
    let index' = (index + i) % Array.length blocks in
    let amt = extra / Array.length blocks in
    if !residue > 0
    then (
      let amt = amt + 1 in
      blocks.( index' ) <- blocks.( index' ) + amt;
      decr residue)
    else (blocks.( index' ) <- blocks.( index' ) + amt)
  done;
  Array.to_list blocks
;;

module Key = struct
  module T = struct
    type t = int list [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make(T)
end

let () =
  let seen = Key.Hash_set.create () in
  let blocks =
    In_channel.with_file (Sys.get_argv ()).(1) ~f:(fun file ->
      In_channel.input_line_exn file
      |> String.split ~on:'\t'
      |> List.map ~f:Int.of_string)
  in
  Hash_set.add seen blocks;
  let rec loop blocks count =
    let blocks = redistribute blocks in
    let count = count + 1 in
    if Hash_set.mem seen blocks
    then count
    else (
      Hash_set.add seen blocks;
      loop blocks count)
  in
  loop blocks 0
  |> printf "%d\n"
;;
