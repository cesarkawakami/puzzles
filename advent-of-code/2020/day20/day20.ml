open Core_kernel

module ArrOps = struct
  let rotate (mtx : 'a array array) : 'a array array =
    let rows = Array.length mtx in
    let cols = Array.length mtx.(0) in
    let result = Array.make_matrix ~dimx:cols ~dimy:rows mtx.(0).(0) in
    for i = 0 to cols - 1 do
      for j = 0 to rows - 1 do
        result.(i).(j) <- mtx.(j).(cols - i - 1)
      done
    done;
    result

  let flip (mtx : 'a array array) : 'a array array =
    let rows = Array.length mtx in
    let cols = Array.length mtx.(0) in
    let result = Array.make_matrix ~dimx:rows ~dimy:cols mtx.(0).(0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        result.(i).(j) <- mtx.(i).(cols - j - 1)
      done
    done;
    result

  let variation_transforms = [ Fn.id; rotate; rotate; rotate; flip; rotate; rotate; rotate ]

  let variations (mtx : 'a array array) : 'a array array Sequence.t =
    Sequence.of_list variation_transforms
    |> Sequence.folding_map ~init:mtx ~f:(fun acc f ->
           let r = f acc in
           (r, r))
end

module Tile = struct
  type t = { id : int; matrix : bool array array } [@@deriving show]

  let nil = { id = -1; matrix = [||] }

  let side (tile : t) : int = Array.length tile.matrix

  let rotate (tile : t) : t = { id = tile.id; matrix = ArrOps.rotate tile.matrix }

  let flip (tile : t) : t = { id = tile.id; matrix = ArrOps.flip tile.matrix }

  let variations (tile : t) : t Sequence.t =
    tile.matrix |> ArrOps.variations |> Sequence.map ~f:(fun mtx -> { id = tile.id; matrix = mtx })

  let compatible_top (tile : t) (other : t) : bool =
    Array.equal Bool.equal tile.matrix.(0) (Array.last other.matrix)

  let compatible_left (tile : t) (other : t) : bool =
    let n = side tile in
    Sequence.range 0 n
    |> Sequence.for_all ~f:(fun i -> Bool.equal tile.matrix.(i).(0) other.matrix.(i).(n - 1))

  let to_string (tile : t) : string =
    tile.matrix
    |> Array.map ~f:(fun row ->
           row
           |> Array.map ~f:(fun v -> if v then '#' else '.')
           |> Array.to_list |> String.of_char_list)
    |> String.concat_array ~sep:"\n"
end

let place_map_to_string (map : Tile.t array array) : string =
  map
  |> Array.map ~f:(fun row ->
         row
         |> Array.map ~f:(fun { Tile.id; _ } -> Int.to_string id)
         |> String.concat_array ~sep:" ")
  |> String.concat_array ~sep:"\n"

let deep_clone arr = arr |> Array.map ~f:(fun row -> Array.copy row)

let rec solve (placed : Tile.t array array) (tile_set : Tile.t Int.Map.t) (board_length : int)
    (cur_i : int) (cur_j : int) : Tile.t array array option =
  (* let () = printf "placed:\n%s\n" (place_map_to_string placed) in *)
  if cur_i = board_length then
    if cur_j <> 0 then failwithf "expected (i, j) = (n, 0) but got (%d, %d)" cur_i cur_j ()
    else Some (deep_clone placed)
  else
    let attempt id cand =
      if cur_i > 0 && not (Tile.compatible_top cand placed.(cur_i - 1).(cur_j)) then None
      else if cur_j > 0 && not (Tile.compatible_left cand placed.(cur_i).(cur_j - 1)) then None
      else (
        placed.(cur_i).(cur_j) <- cand;
        let next_i, next_j =
          if cur_j = board_length - 1 then (cur_i + 1, 0) else (cur_i, cur_j + 1)
        in
        let subresult = solve placed (Int.Map.remove tile_set id) board_length next_i next_j in
        placed.(cur_i).(cur_j) <- Tile.nil;
        subresult )
    in

    tile_set |> Int.Map.to_sequence
    |> Sequence.concat_map ~f:(fun (id, cand) ->
           Tile.variations cand |> Sequence.map ~f:(fun cand -> (id, cand)))
    |> Sequence.fold_until ~init:()
         ~finish:(fun _ -> None)
         ~f:(fun _ (id, cand) ->
           let subresult = attempt id cand in
           if Option.is_some subresult then Stop subresult else Continue ())

let parse_tile (lines : string list) : Tile.t =
  match lines with
  | title_line :: matrix_lines ->
      let tile_id = Scanf.sscanf title_line "Tile %d" Fn.id in
      let tile_matrix =
        matrix_lines |> List.to_array
        |> Array.map ~f:(fun line ->
               line |> String.to_array
               |> Array.map ~f:(fun ch ->
                      match ch with
                      | '.' -> false
                      | '#' -> true
                      | _ -> failwithf "unexpected char %c" ch ()))
      in
      { id = tile_id; matrix = tile_matrix }
  | [] -> failwith "unexpected empty list when parsing tile"

let rec parse_tile_set (lines : string list) : Tile.t Int.Map.t =
  let lines = lines |> List.drop_while ~f:String.is_empty in
  if List.is_empty lines then Int.Map.empty
  else
    let tile_lines, rest = lines |> List.split_while ~f:(Fn.non String.is_empty) in
    let tile = parse_tile tile_lines in
    let subresult = parse_tile_set rest in
    subresult |> Int.Map.set ~key:tile.id ~data:tile

let build_image (board_length : int) (n : int) (solution : Tile.t array array) : bool array array =
  let final_tile_side = n - 2 in
  let final_image_side = board_length * final_tile_side in
  let result = Array.make_matrix ~dimx:final_image_side ~dimy:final_image_side false in
  for ti = 0 to board_length - 1 do
    for tj = 0 to board_length - 1 do
      let start_i = ti * final_tile_side in
      let start_j = tj * final_tile_side in
      for i = 0 to final_tile_side - 1 do
        for j = 0 to final_tile_side - 1 do
          result.(start_i + i).(start_j + j) <- solution.(ti).(tj).Tile.matrix.(i + 1).(j + 1)
        done
      done
    done
  done;
  result

let sea_monster_str = [| "                  # "; "#    ##    ##    ###"; " #  #  #  #  #  #   " |]

let sea_monster =
  sea_monster_str
  |> Array.map ~f:(fun line ->
         line |> String.to_array
         |> Array.map ~f:(fun ch ->
                match ch with
                | '#' -> true
                | ' ' -> false
                | _ -> failwithf "unexpected char: %c" ch ()))

let find_sea_monsters (image : bool array array) : bool array array =
  let n = Array.length image in
  let result = Array.make_matrix ~dimx:n ~dimy:n false in
  assert (n = Array.length image.(0));

  let go sea_monster =
    let monster_rows = Array.length sea_monster in
    let monster_cols = Array.length sea_monster.(0) in

    let test si sj =
      let found = ref true in
      for i = 0 to monster_rows - 1 do
        for j = 0 to monster_cols - 1 do
          if sea_monster.(i).(j) && not image.(si + i).(sj + j) then found := false else ()
        done
      done;
      if !found then
        for i = 0 to monster_rows - 1 do
          for j = 0 to monster_cols - 1 do
            if sea_monster.(i).(j) then result.(si + i).(sj + j) <- true else ()
          done
        done
      else ()
    in

    for i = 0 to n - 1 - monster_rows do
      for j = 0 to n - 1 - monster_cols do
        test i j
      done
    done
  in

  ArrOps.variations sea_monster |> Sequence.iter ~f:go;
  result

let sea_monster_image_to_string (image : bool array array) (sea_monsters : bool array array) :
    string =
  let merge_cell (image_cell, sea_cell) =
    match (image_cell, sea_cell) with _, true -> 'O' | true, false -> '#' | false, false -> '.'
  in
  let merge_rows (image_row, sea_row) =
    image_row |> Array.zip_exn sea_row |> Array.map ~f:merge_cell |> Array.to_list
    |> String.of_char_list
  in
  image |> Array.zip_exn sea_monsters |> Array.map ~f:merge_rows |> String.concat_array ~sep:"\n"

let () =
  let lines = In_channel.input_lines Stdio.stdin |> List.map ~f:String.strip in
  let tile_set = parse_tile_set lines in
  let n = Int.Map.nth tile_set 0 |> Option.value_exn |> fun (_, tile) -> Tile.side tile in
  let tile_count = Int.Map.length tile_set in
  let board_length = Int.of_float (Float.sqrt (Float.of_int tile_count)) in
  let () = assert (board_length * board_length = tile_count) in
  let () = printf "n: %d\n" n in
  let () = printf "board_length: %d\n" board_length in

  let empty_placed = Array.make_matrix ~dimx:board_length ~dimy:board_length Tile.nil in
  let solution = solve empty_placed tile_set board_length 0 0 |> Option.value_exn in
  let corner_tile_ids =
    [
      solution.(0).(0).Tile.id;
      solution.(0).(board_length - 1).Tile.id;
      solution.(board_length - 1).(0).Tile.id;
      solution.(board_length - 1).(board_length - 1).Tile.id;
    ]
  in
  let () = printf "part1, corners: %s\n" (corner_tile_ids |> List.to_string ~f:Int.to_string) in
  let () = printf "part1, answer: %d\n" (corner_tile_ids |> List.reduce_exn ~f:Int.( * )) in
  printf "\n";

  let image = build_image board_length n solution in
  let sea_monsters = find_sea_monsters image in
  let rendered = sea_monster_image_to_string image sea_monsters in
  printf "part2, image:\n";
  printf "%s\n" rendered;
  printf "\n";

  let roughness = rendered |> String.count ~f:(fun ch -> Char.(ch = '#')) in
  printf "part2, answer: %d\n" roughness;

  ()
