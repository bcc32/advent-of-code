open! Core

let () =
  let lengths =
    In_channel.with_file
      (Sys.get_argv ()).(1)
      ~f:(fun file ->
        In_channel.input_all file
        |> String.strip
        |> String.to_list
        |> List.map ~f:Char.to_int)
    @ [ 17; 31; 73; 47; 23 ]
  in
  let list = Array.init 256 ~f:Fn.id in
  let pos = ref 0 in
  let skip = ref 0 in
  for _ = 1 to 64 do
    List.iter lengths ~f:(fun len ->
      for i = 0 to (len / 2) - 1 do
        let left = !pos + i in
        let right = !pos + (len - i - 1) in
        let left, right = left % 256, right % 256 in
        let t = list.(left) in
        list.(left) <- list.(right);
        list.(right) <- t
      done;
      pos := !pos + len + !skip;
      incr skip)
  done;
  List.range 0 16
  |> List.map ~f:(fun i -> Array.sub list ~pos:(16 * i) ~len:16)
  |> List.map ~f:(Array.fold ~init:0 ~f:( lxor ))
  |> List.iter ~f:(printf "%02x");
  printf "\n"
;;
