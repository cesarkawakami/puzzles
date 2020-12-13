open Core_kernel

module CharSet = Set.Make (Char)

let group_count_pt1 group_string =
  let persons = String.split_lines group_string in
  let answer_sets = List.map ~f:(fun p -> CharSet.of_list (String.to_list p)) persons in
  let union = List.fold ~init:CharSet.empty ~f:CharSet.union answer_sets in
  CharSet.length union

let group_count_pt2 group_string =
  let persons = String.split_lines group_string in
  let answer_sets = List.map ~f:(fun p -> CharSet.of_list (String.to_list p)) persons in
  let intersection = List.fold ~init:(Option.value_exn (List.hd answer_sets)) ~f:CharSet.inter answer_sets in
  CharSet.length intersection

let () =
  let input = In_channel.input_all Stdio.stdin in
  let group_strings = Str.split (Str.regexp_string "\n\n") input in
  let group_count_sum = List.sum (module Int) ~f:group_count_pt1 group_strings in
  let () = printf "part1: %d\n" group_count_sum in
  let group_count_sum_pt2 = List.sum (module Int) ~f:group_count_pt2 group_strings in
  let () = printf "part2: %d\n" group_count_sum_pt2 in
  ()
