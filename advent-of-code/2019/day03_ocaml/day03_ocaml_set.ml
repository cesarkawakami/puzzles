open Core_kernel

type move_t = MRight of int | MDown of int | MLeft of int | MUp of int

module Coord = struct
  module T = struct
    type t = Coord of int * int [@@deriving hash, sexp, compare]
  end

  include T
  include Hashable.Make (T)
  module Set = Set.Make (T)
end

let wire_coords (moves : move_t list) : Coord.Set.t =
  let rec go acc x y moves =
    match moves with
    | [] -> Coord.Set.add acc (Coord.Coord (x, y))
    | MRight 0 :: tl -> go acc x y tl
    | MDown 0 :: tl -> go acc x y tl
    | MLeft 0 :: tl -> go acc x y tl
    | MUp 0 :: tl -> go acc x y tl
    | MRight v :: tl -> go (Coord.Set.add acc (Coord.Coord (x, y))) (x + 1) y (MRight (v - 1) :: tl)
    | MDown v :: tl -> go (Coord.Set.add acc (Coord.Coord (x, y))) x (y - 1) (MDown (v - 1) :: tl)
    | MLeft v :: tl -> go (Coord.Set.add acc (Coord.Coord (x, y))) (x - 1) y (MLeft (v - 1) :: tl)
    | MUp v :: tl -> go (Coord.Set.add acc (Coord.Coord (x, y))) x (y + 1) (MUp (v - 1) :: tl)
  in
  go Coord.Set.empty 0 0 moves

let parse_move (s : string) : move_t =
  let conv lst = String.of_char_list lst |> Int.of_string in
  match String.to_list s with
  | 'R' :: tl -> MRight (conv tl)
  | 'D' :: tl -> MDown (conv tl)
  | 'L' :: tl -> MLeft (conv tl)
  | 'U' :: tl -> MUp (conv tl)
  | _ -> failwithf "not a move: %s" s ()

let parse_wire (s : string) : move_t list =
  s |> String.strip |> String.split ~on:',' |> List.map ~f:parse_move

let parse_input (s : string) : move_t list * move_t list =
  match String.split_lines s with
  | [ line1; line2 ] -> (parse_wire line1, parse_wire line2)
  | _ -> failwithf "can't parse %s" s ()

(* let manhattan ((x, y) : int * int) : int = Int.abs x + Int.abs y *)
let manhattan (Coord (x, y) : Coord.t) : int = Int.abs x + Int.abs y

let compare_pair ((left_a, left_b) : int * int) ((right_a, right_b) : int * int) : int =
  if left_a <> right_a then compare left_a right_a else compare left_b right_b

(* let compare_manhattan (left : int * int) (right : int * int) : int =
  compare (manhattan left) (manhattan right) *)
let compare_manhattan (left : Coord.t) (right : Coord.t) : int =
  compare (manhattan left) (manhattan right)

(* let sorted_inters (left : (int * int) list) (right : (int * int) list) : (int * int) list =
  let rec go acc left right =
    match (left, right) with
    | [], _ -> acc
    | _, [] -> acc
    | left_val :: left_tail, right_val :: right_tail ->
        let compare_result = compare_pair left_val right_val in
        if compare_result < 0 then go acc left_tail (right_val :: right_tail)
        else if compare_result > 0 then go acc (left_val :: left_tail) right_tail
        else go (left_val :: acc) left_tail right_tail
  in
  go [] left right *)

let do_part1 (input : string) : unit =
  let moves1, moves2 = parse_input input in
  (* let coords1 = List.drop (wire_coords moves1) 1 |> List.sort ~compare:compare_pair in
     let coords2 = List.drop (wire_coords moves2) 1 |> List.sort ~compare:compare_pair in
     let inters = sorted_inters coords1 coords2 in *)
  let coords1 = Coord.Set.remove (wire_coords moves1) (Coord.Coord (0, 0)) in
  let coords2 = Coord.Set.remove (wire_coords moves2) (Coord.Coord (0, 0)) in
  let inters = Coord.Set.inter coords1 coords2 |> Coord.Set.to_list in
  let (Coord.Coord (closestX, closestY)) =
    inters |> List.min_elt ~compare:compare_manhattan |> Option.value_exn
  in
  let closestManhattan = manhattan (Coord.Coord (closestX, closestY)) in
  printf "part1, p: (%d, %d), dist: %d\n" closestX closestY closestManhattan

let benchtest () =
  let input = In_channel.read_all "input" in
  do_part1 input

(* let () =
  let bench_res = Benchmark.throughput1 10 benchtest () in
  Benchmark.tabulate bench_res *)
