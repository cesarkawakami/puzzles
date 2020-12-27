open Core_kernel

module Dir = struct
  type dir_t = E | SE | SW | W | NW | NE

  let list_of_line line =
    let rec go acc line =
      match line with
      | [] -> List.rev acc
      | 'e' :: rest -> go (E :: acc) rest
      | 's' :: 'e' :: rest -> go (SE :: acc) rest
      | 's' :: 'w' :: rest -> go (SW :: acc) rest
      | 'n' :: 'e' :: rest -> go (NE :: acc) rest
      | 'n' :: 'w' :: rest -> go (NW :: acc) rest
      | 'w' :: rest -> go (W :: acc) rest
      | _ -> failwithf "cant parse %s" (line |> List.to_string ~f:Char.to_string) ()
    in
    go [] (String.to_list line)

  let all = [ E; SE; SW; W; NW; NE ]
end

module Coord = struct
  module T = struct
    type t = int * int [@@deriving hash, compare, sexp]
  end

  include Hashable.Make (T)

  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let of_dir dir =
    match dir with
    | Dir.E -> (1, 0)
    | Dir.NE -> (0, 1)
    | Dir.NW -> (-1, 1)
    | Dir.W -> (-1, 0)
    | Dir.SW -> (0, -1)
    | Dir.SE -> (1, -1)

  let of_dir_list dir_list = dir_list |> List.fold ~init:(0, 0) ~f:(fun acc dir -> acc + of_dir dir)

  let of_line line = line |> Dir.list_of_line |> of_dir_list

  let neighbors p = Dir.all |> List.map ~f:(fun dir -> p + of_dir dir)

  let incl_neighbors p = p :: neighbors p
end

module State = struct
  type t = Coord.Hash_set.t

  type color_t = Active | Inactive

  let create () = Coord.Hash_set.create ()

  let active_count = Hash_set.length

  let relevant_coords state =
    state |> Hash_set.to_list
    |> List.concat_map ~f:(fun p -> Coord.incl_neighbors p)
    |> List.fold ~init:(Coord.Hash_set.create ()) ~f:(fun acc p ->
           Hash_set.add acc p;
           acc)
    |> Hash_set.to_list

  let color state p = if Hash_set.mem state p then Active else Inactive

  let is_active state p = match color state p with Active -> true | Inactive -> false

  let is_inactive state p = not (is_active state p)

  let relevant_with_colors state =
    relevant_coords state |> List.map ~f:(fun p -> (p, color state p))

  let switch state p =
    if Hash_set.mem state p then Hash_set.remove state p else Hash_set.add state p

  let next_day_p state p =
    let active_neigh_count =
      Coord.neighbors p |> List.filter ~f:(fun p -> is_active state p) |> List.length
    in
    match color state p with
    | Active -> if active_neigh_count = 0 || active_neigh_count > 2 then Inactive else Active
    | Inactive -> if active_neigh_count = 2 then Active else Inactive

  let next_day state =
    state |> relevant_coords
    |> List.map ~f:(fun p -> (p, next_day_p state p))
    |> List.filter ~f:(fun (_, color) -> match color with Active -> true | Inactive -> false)
    |> List.map ~f:(fun (p, _) -> p)
    |> Coord.Hash_set.of_list
end

let solve_part1 lines =
  let state = State.create () in
  let points = lines |> List.map ~f:Coord.of_line in
  points |> List.iter ~f:(State.switch state);
  printf "part1, active count: %d\n" (State.active_count state)

let solve_part2 lines =
  let initial_state =
    let state = State.create () in
    let points = lines |> List.map ~f:Coord.of_line in
    points |> List.iter ~f:(State.switch state);
    state
  in
  printf "part2, day  0: %d\n" (State.active_count initial_state);
  let final_state =
    List.range ~stop:`inclusive 1 100
    |> List.fold ~init:initial_state ~f:(fun state day ->
           let next_state = State.next_day state in
           printf "part2, day%3d: %d\n" day (State.active_count next_state);
           next_state)
  in
  printf "part2, answer: %d\n" (State.active_count final_state)

let () =
  let lines = In_channel.input_lines Stdio.stdin in
  solve_part1 lines;
  solve_part2 lines
