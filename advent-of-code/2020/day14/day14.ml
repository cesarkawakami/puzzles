open Core_kernel

type tritmask_t = Int63.t * Int63.t * Int63.t

type instruction_t = SetMask of tritmask_t | SetMemoryPosition of Int63.t * Int63.t

type machine_state_t = tritmask_t * Int63.t Int63.Map.t

let dump = BatPervasives.dump

let apply_tritmask (value : Int63.t) (mask : tritmask_t) : Int63.t =
  let positive, negative, _ = mask in
  Int63.(value land lnot negative lor positive)

let tritmask_of_string (s : string) : tritmask_t =
  String.fold s
    ~init:Int63.(zero, zero, zero)
    ~f:(fun (acc_pos, acc_neg, acc_float) ch ->
      let cur_pos = match ch with '1' -> Int63.one | _ -> Int63.zero in
      let cur_neg = match ch with '0' -> Int63.one | _ -> Int63.zero in
      let cur_float = match ch with 'X' -> Int63.one | _ -> Int63.zero in
      Int63.
        ((acc_pos lsl 1) lor cur_pos, (acc_neg lsl 1) lor cur_neg, (acc_float lsl 1) lor cur_float))

let rec generate_positions (center : Int63.t) (mask : tritmask_t) : Int63.t Sequence.t =
  let mask_one, mask_zero, mask_float = mask in
  if Int63.(center = zero && mask_one = zero && mask_zero = zero && mask_float = zero) then
    Sequence.return Int63.zero
  else
    let center_last, mask_one_last, mask_zero_last, mask_float_last =
      Int63.(center land one, mask_one land one, mask_zero land one, mask_float land one)
    in
    let center_rest, mask_one_rest, mask_zero_rest, mask_float_rest =
      Int63.(center asr 1, mask_one asr 1, mask_zero asr 1, mask_float asr 1)
    in
    let subresult =
      generate_positions center_rest (mask_one_rest, mask_zero_rest, mask_float_rest)
    in
    let shifted = subresult |> Sequence.map ~f:(fun p -> Int63.(p lsl 1)) in
    if Int63.(mask_zero_last = one) then
      shifted |> Sequence.map ~f:(fun p -> Int63.(p lor center_last))
    else if Int63.(mask_one_last = one) then shifted |> Sequence.map ~f:(fun p -> Int63.(p lor one))
    else if Int63.(mask_float_last = one) then
      Sequence.append
        (shifted |> Sequence.map ~f:(fun p -> Int63.(p lor zero)))
        (shifted |> Sequence.map ~f:(fun p -> Int63.(p lor one)))
    else failwith "invalid tritmask?"

let apply_instruction (state : machine_state_t) (instruction : instruction_t) : machine_state_t =
  let tritmask, memory_state = state in
  match instruction with
  | SetMask new_mask -> (new_mask, memory_state)
  | SetMemoryPosition (position, value) ->
      let new_value = apply_tritmask value tritmask in
      let new_memory_state = Int63.Map.set memory_state ~key:position ~data:new_value in
      (tritmask, new_memory_state)

let apply_instruction_2 (state : machine_state_t) (instruction : instruction_t) : machine_state_t =
  let tritmask, memory_state = state in
  match instruction with
  | SetMask new_mask -> (new_mask, memory_state)
  | SetMemoryPosition (center, value) ->
      let affected_positions = generate_positions center tritmask in
      let new_memory_state =
        affected_positions
        |> Sequence.fold ~init:memory_state ~f:(fun prev_state position ->
               Int63.Map.set prev_state ~key:position ~data:value)
      in
      (tritmask, new_memory_state)

let parse_mask_line (mask_line : string) : instruction_t =
  match String.split ~on:' ' (String.strip mask_line) with
  | [ "mask"; "="; tritmask_string ] -> SetMask (tritmask_of_string tritmask_string)
  | _ -> failwithf "unable to parse mask line: %s" mask_line ()

let instruction_re = Re2.create_exn "mem\\[(\\d+)\\] = (\\d+)"

let parse_instruction_line (instruction_line : string) : instruction_t =
  match Re2.first_match instruction_re instruction_line with
  | Ok match_ -> (
      match Re2.Match.get_all (Re2.without_trailing_none match_) with
      | [| _; Some pos_str; Some value_str |] ->
          SetMemoryPosition (Int63.of_string pos_str, Int63.of_string value_str)
      | _ -> failwithf "error extracting matches from %s" (dump match_) () )
  | Error _ -> failwithf "unable to parse instruction: %s" instruction_line ()

let parse_line (line : string) : instruction_t =
  match String.prefix line 3 with
  | "mas" -> parse_mask_line line
  | "mem" -> parse_instruction_line line
  | _ -> failwithf "unable to parse: %s" line ()

let sum_memory (machine_state : machine_state_t) : Int63.t =
  let _, memory_state = machine_state in
  memory_state |> Int63.Map.fold ~init:Int63.zero ~f:(fun ~key:_key ~data acc -> Int63.(acc + data))

let print_memory (machine_state : machine_state_t) : unit =
  let _, memory_state = machine_state in
  memory_state
  |> Int63.Map.iteri ~f:(fun ~key ~data ->
         printf "  %4s: %6s\n" (Int63.to_string key) (Int63.to_string data))

let _ =
  let lines = In_channel.input_lines Stdio.stdin in
  let instructions = lines |> List.map ~f:parse_line in

  let initial_machine_state = ((Int63.zero, Int63.zero, Int63.zero), Int63.Map.empty) in
  let final_machine_state =
    instructions |> List.fold ~init:initial_machine_state ~f:apply_instruction
  in
  let memory_sum = sum_memory final_machine_state in
  let _ = printf "part1, final machine memory:\n" in
  let _ = print_memory final_machine_state in
  let _ = printf "part1, final memory sum: %s\n" (Int63.to_string memory_sum) in

  let final_machine_state_2 =
    instructions |> List.fold ~init:initial_machine_state ~f:apply_instruction_2
  in
  let memory_sum_2 = sum_memory final_machine_state_2 in
  let _ = printf "part2, final machine memory:\n" in
  let _ = print_memory final_machine_state_2 in
  let _ = printf "part2, final memory sum: %s\n" (Int63.to_string memory_sum_2) in

  ()
