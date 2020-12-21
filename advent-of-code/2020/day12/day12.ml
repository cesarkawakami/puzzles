open Core_kernel

type instruction_t =
  | North of int
  | South of int
  | East of int
  | West of int
  | Left of int
  | Right of int
  | Forward of int

type state_t = { position : int * int; direction : int * int } [@@deriving show]

let vector_to_angle ((x, y) : int * int) : int =
  match (x, y) with
  | 1, 0 -> 0
  | 0, 1 -> 90
  | -1, 0 -> 180
  | 0, -1 -> 270
  | _ -> failwithf "unexpected input direction: (%d, %d)" x y ()

let angle_to_vector (angle : int) : int * int =
  match angle with
  | 0 -> (1, 0)
  | 90 -> (0, 1)
  | 180 -> (-1, 0)
  | 270 -> (0, -1)
  | _ -> failwithf "unexpected input angle: %d" angle ()

let rotate ((x, y) : int * int) (angle : int) : int * int =
  let vec_angle = vector_to_angle (x, y) in
  let new_angle = (((vec_angle + angle) % 360) + 360) % 360 in
  angle_to_vector new_angle

let rotate2 ((x, y) : int * int) (angle : int) : int * int =
  let angle = ((angle % 360) + 360) % 360 in
  match angle with
  | 0 -> (x, y)
  | 90 -> (-y, x)
  | 180 -> (-x, -y)
  | 270 -> (y, -x)
  | _ -> failwithf "unrecognized angle: %d" angle ()

let apply_instruction (current_state : state_t) (instruction : instruction_t) : state_t =
  let { position = px, py; direction = dx, dy } = current_state in
  let _ = printf "%s\n" (show_state_t current_state) in
  match instruction with
  | North v -> { current_state with position = (px, py + v) }
  | South v -> { current_state with position = (px, py - v) }
  | East v -> { current_state with position = (px + v, py) }
  | West v -> { current_state with position = (px - v, py) }
  | Left v -> { current_state with direction = rotate (dx, dy) v }
  | Right v -> { current_state with direction = rotate (dx, dy) (-v) }
  | Forward v -> { current_state with position = (px + (v * dx), py + (v * dy)) }

let apply_instruction_2 (current_state : state_t) (instruction : instruction_t) : state_t =
  let { position = px, py; direction = dx, dy } = current_state in
  let _ = printf "%s\n" (show_state_t current_state) in
  match instruction with
  | North v -> { current_state with direction = (dx, dy + v) }
  | South v -> { current_state with direction = (dx, dy - v) }
  | East v -> { current_state with direction = (dx + v, dy) }
  | West v -> { current_state with direction = (dx - v, dy) }
  | Left v -> { current_state with direction = rotate2 (dx, dy) v }
  | Right v -> { current_state with direction = rotate2 (dx, dy) (-v) }
  | Forward v -> { current_state with position = (px + (v * dx), py + (v * dy)) }

let apply_instructions (initial_state : state_t) (instructions : instruction_t list) : state_t =
  List.fold ~init:initial_state ~f:apply_instruction instructions

let apply_instructions_2 (initial_state : state_t) (instructions : instruction_t list) : state_t =
  List.fold ~init:initial_state ~f:apply_instruction_2 instructions

let parse_instruction (raw_instruction : string) : instruction_t =
  let action_char = raw_instruction.[0] in
  let value = Int.of_string (String.suffix raw_instruction (String.length raw_instruction - 1)) in
  match action_char with
  | 'N' -> North value
  | 'S' -> South value
  | 'E' -> East value
  | 'W' -> West value
  | 'L' -> Left value
  | 'R' -> Right value
  | 'F' -> Forward value
  | _ -> failwithf "unable to parse instruction: %s" raw_instruction ()

let manhattan_distance ((x, y) : int * int) : int = Int.abs x + Int.abs y

let _ =
  let lines = In_channel.input_lines Stdio.stdin in
  let raw_instructions = List.map ~f:(fun line -> String.strip line) lines in
  let instructions = List.map ~f:parse_instruction raw_instructions in
  let initial_state = { position = (0, 0); direction = (1, 0) } in
  let final_state = apply_instructions initial_state instructions in
  let _ = printf "part1, final state: %s\n" (show_state_t final_state) in
  let _ = printf "part1, manhattan: %d\n" (manhattan_distance final_state.position) in
  let initial_state_2 = { position = (0, 0); direction = (10, 1) } in
  let final_state_2 = apply_instructions_2 initial_state_2 instructions in
  let _ = printf "part2, final state: %s\n" (show_state_t final_state_2) in
  let _ = printf "part2, manhattan: %d\n" (manhattan_distance final_state_2.position) in
  ()
