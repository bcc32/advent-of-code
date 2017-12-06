open! Core

let redistribute blocks =
  let index =
    Sequence.zip (Sequence.range 0 (List.length blocks)) (Sequence.of_list blocks)
    |> Sequence.min_elt ~cmp:(fun (i, x) (j, y) ->
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
    type t = int list [@@deriving sexp]

    let compare = [%compare: int list]

    let hash = [%hash: int list]
  end

  include T
  include Hashable.Make(T)
end

let () =
  let seen = Key.Table.create () in
  let blocks =
    In_channel.with_file Sys.argv.(1) ~f:(fun file ->
      In_channel.input_line_exn file
      |> String.split ~on:'\t'
      |> List.map ~f:Int.of_string)
  in
  with_return (fun { return } ->
    let cycle = ref 0 in
    let record blocks =
      let last = Hashtbl.find_or_add seen blocks ~default:(fun () -> !cycle) in
      if last < !cycle
      then (return (!cycle - last))
      else (incr cycle)
    in
    let rec loop blocks =
      let blocks = redistribute blocks in
      record blocks;
      loop blocks
    in
    loop blocks)
  |> printf "%d\n"
;;
