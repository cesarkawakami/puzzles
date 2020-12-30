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

(* let for_each_wire_coord (moves : move_t list) (f : Coord.t -> unit) : unit =
  let rec go x y moves =
    match moves with
    | [] -> f (Coord.Coord (x, y))
    | MRight 0 :: tl -> go x y tl
    | MDown 0 :: tl -> go x y tl
    | MLeft 0 :: tl -> go x y tl
    | MUp 0 :: tl -> go x y tl
    | MRight v :: tl ->
        f (Coord.Coord (x, y));
        go (x + 1) y (MRight (v - 1) :: tl)
    | MDown v :: tl ->
        f (Coord.Coord (x, y));
        go x (y - 1) (MDown (v - 1) :: tl)
    | MLeft v :: tl ->
        f (Coord.Coord (x, y));
        go (x - 1) y (MLeft (v - 1) :: tl)
    | MUp v :: tl ->
        f (Coord.Coord (x, y));
        go x (y + 1) (MUp (v - 1) :: tl)
  in
  go 0 0 moves *)

let for_each_wire_coord (moves : move_t list) (f : Coord.t -> unit) : unit =
  let rec go x y moves =
    match moves with
    | [] -> f (Coord.Coord (x, y))
    | MRight 0 :: tl -> go x y tl
    | MDown 0 :: tl -> go x y tl
    | MLeft 0 :: tl -> go x y tl
    | MUp 0 :: tl -> go x y tl
    | MRight v :: tl -> go2 x y 1 0 v tl
    | MDown v :: tl -> go2 x y 0 (-1) v tl
    | MLeft v :: tl -> go2 x y (-1) 0 v tl
    | MUp v :: tl -> go2 x y 0 1 v tl
  and go2 x y dx dy t remaining =
    if t = 0 then go x y remaining
    else (
      f (Coord.Coord (x, y));
      go2 (x + dx) (y + dy) dx dy (t - 1) remaining )
  in
  go 0 0 moves

let wire_inters (left : move_t list) (right : move_t list) : Coord.t list =
  let left_set = Coord.Hash_set.create ~size:200000 () in
  for_each_wire_coord left (fun coord -> Hash_set.add left_set coord);
  Hash_set.remove left_set (Coord.Coord (0, 0));

  let output = ref [] in
  for_each_wire_coord right (fun coord ->
      if Hash_set.mem left_set coord then output := coord :: !output else ());
  !output

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

let manhattan (Coord (x, y) : Coord.t) : int = Int.abs x + Int.abs y

let compare_pair ((left_a, left_b) : int * int) ((right_a, right_b) : int * int) : int =
  if left_a <> right_a then compare left_a right_a else compare left_b right_b

let compare_manhattan (left : Coord.t) (right : Coord.t) : int =
  compare (manhattan left) (manhattan right)

let do_part1 (input : string) : unit =
  let moves1, moves2 = parse_input input in
  let inters = wire_inters moves1 moves2 in
  let (Coord.Coord (closestX, closestY)) =
    inters |> List.min_elt ~compare:compare_manhattan |> Option.value_exn
  in
  let closestManhattan = manhattan (Coord.Coord (closestX, closestY)) in
  printf "part1, p: (%d, %d), dist: %d\n" closestX closestY closestManhattan

let benchtest () =
  let input = In_channel.read_all "input" in
  do_part1 input
