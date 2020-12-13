open Core_kernel
module IntSet = Set.Make (Int)

type instruction_t = Acc of int | Jmp of int | Nop of int [@@deriving show]

type interpreter_state_t = { ip : int; acc : int } [@@deriving show]

let line_to_instruction (line : string) =
  match String.split ~on:' ' line with
  | [ "acc"; data ] -> Acc (Int.of_string data)
  | [ "jmp"; data ] -> Jmp (Int.of_string data)
  | [ "nop"; data ] -> Nop (Int.of_string data)
  | _ -> failwithf "error parsing: %s" line ()

let interpret_instruction (state : interpreter_state_t)
    (instruction : instruction_t) =
  match instruction with
  | Acc delta -> { ip = state.ip + 1; acc = state.acc + delta }
  | Jmp jump_size -> { ip = state.ip + jump_size; acc = state.acc }
  | Nop _data -> { ip = state.ip + 1; acc = state.acc }

let rec run_until_ip_repeat_or_end (program : instruction_t array)
    (state : interpreter_state_t) (seen_ips : IntSet.t) =
  if IntSet.mem seen_ips state.ip || Int.equal state.ip (Array.length program)
  then state
  else
    (* let () = printf "debug: %s\n" (show_interpreter_state_t state) in *)
    let current_instruction = program.(state.ip) in
    let next_state = interpret_instruction state current_instruction in
    let next_seen_ips = IntSet.add seen_ips state.ip in
    run_until_ip_repeat_or_end program next_state next_seen_ips

let mutate (instruction : instruction_t) =
  match instruction with
  | Acc delta -> Acc delta
  | Jmp jump_size -> Nop jump_size
  | Nop data -> Jmp data

let rec find_mutation (program : instruction_t array) (pos : int) =
  if Int.equal (Array.length program) pos then failwithf "mutation not found" ()
  else program.(pos) <- mutate program.(pos);
  let last_state =
    run_until_ip_repeat_or_end program { ip = 0; acc = 0 } IntSet.empty
  in
  program.(pos) <- mutate program.(pos);
  if Int.equal last_state.ip (Array.length program) then (pos, last_state)
  else find_mutation program (pos + 1)

let () =
  let lines = Array.of_list (In_channel.input_lines Stdio.stdin) in
  let program = Array.map ~f:line_to_instruction lines in
  let part1_final_state =
    run_until_ip_repeat_or_end program { ip = 0; acc = 0 } IntSet.empty
  in
  let () = printf "part1: %d\n" part1_final_state.acc in
  let part2_pos, part2_last_state = find_mutation program 0 in
  let () =
    printf "part2: pos=%d state=%s\n" part2_pos
      (show_interpreter_state_t part2_last_state)
  in
  ()
