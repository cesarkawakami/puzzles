open Core_kernel
module IntMap = Map.Make (Int)

let compute_arrangements adapters =
  let adapters = Array.of_list adapters in
  let cache = Array.create ~len:(Array.length adapters) None in
  let rec compute n =
    let compute_impl n =
      let rec do_single reference n acc =
        if n >= 0 && reference - adapters.(n) <= 3 then
          do_single reference (n - 1) Int63.(acc + compute n)
        else acc
      in
      match n with
      | 0 -> Int63.one
      | n -> do_single adapters.(n) (n - 1) Int63.zero
    in
    match cache.(n) with
    | Some v -> v
    | None ->
        let v = compute_impl n in
        cache.(n) <- Some v;
        v
  in
  let answer = compute (Array.length adapters - 1) in
  let _ =
    Array.iteri cache ~f:(fun i v ->
        printf "D %d -> %s\n" i
          (match v with Some v -> Int63.to_string v | None -> "None"))
  in
  answer

let () =
  let lines = In_channel.input_lines Stdio.stdin in
  let adapters = List.map ~f:Int.of_string lines in
  let sorted_adapters = List.sort ~compare:Int.compare adapters in
  let sorted_adapters =
    0 :: (sorted_adapters @ [ List.last_exn sorted_adapters + 3 ])
  in
  let adapter_pairs =
    List.zip_exn
      (List.take sorted_adapters (List.length sorted_adapters - 1))
      (List.drop sorted_adapters 1)
  in
  let differences = List.map ~f:(fun (a, b) -> b - a) adapter_pairs in
  let diff_counts =
    let increment_map map key =
      IntMap.set map ~key
        ~data:
          (match IntMap.find map key with None -> 1 | Some count -> 1 + count)
    in
    List.fold ~init:IntMap.empty ~f:increment_map differences
  in
  let _ = printf "part1, counts:\n" in
  let _ =
    IntMap.iteri ~f:(fun ~key ~data -> printf "%d -> %d\n" key data) diff_counts
  in
  let _ =
    printf "part1, answer: %d\n"
      (IntMap.find_exn diff_counts 1 * IntMap.find_exn diff_counts 3)
  in
  let _ =
    printf "part2, answer: %s\n"
      (Int63.to_string (compute_arrangements sorted_adapters))
  in
  ()
