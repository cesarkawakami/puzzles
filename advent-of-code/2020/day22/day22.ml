open Core_kernel

module HandPair = struct
  module T = struct
    type t = int list * int list [@@deriving sexp, compare, hash]
  end

  include Hashable.Make (T)
end

type winner_t = Player1 | Player2

module SimResult = struct
  type t = { player1 : int list; player2 : int list; winner : winner_t }

  let of_hands player1 player2 =
    { player1; player2; winner = (if List.is_empty player1 then Player2 else Player1) }

  let of_hands_winner player1 player2 winner = { player1; player2; winner }

  let winner_hand res = match res.winner with Player1 -> res.player1 | Player2 -> res.player2
end

let rec simulate player1 player2 =
  match (player1, player2) with
  | [], _ -> SimResult.of_hands_winner player1 player2 Player2
  | _, [] -> SimResult.of_hands_winner player1 player2 Player1
  | p1card :: p1rest, p2card :: p2rest ->
      if p1card > p2card then simulate (p1rest @ [ p1card; p2card ]) p2rest
      else simulate p1rest (p2rest @ [ p2card; p1card ])

let cache : SimResult.t HandPair.Table.t = HandPair.Table.create ()

let rec simulate2 player1 player2 =
  match HandPair.Table.find cache (player1, player2) with
  | Some result -> result
  | None ->
      (* printf "==> p1: %s\n" (player1 |> List.to_string ~f:Int.to_string);
         printf "==> p2: %s\n" (player2 |> List.to_string ~f:Int.to_string); *)
      let seen_states = HandPair.Hash_set.create () in

      let rec go player1 player2 =
        (* printf "p1: %s\n" (player1 |> List.to_string ~f:Int.to_string);
           printf "p2: %s\n" (player2 |> List.to_string ~f:Int.to_string); *)
        match (player1, player2, Hash_set.mem seen_states (player1, player2)) with
        | [], _, _ -> SimResult.of_hands_winner player1 player2 Player2
        | _, [], _ -> SimResult.of_hands_winner player1 player2 Player1
        | _, _, true -> SimResult.of_hands_winner player1 player2 Player1
        | p1card :: p1rest, p2card :: p2rest, false ->
            Hash_set.add seen_states (player1, player2);

            let continue winner =
              match winner with
              | Player1 -> go (p1rest @ [ p1card; p2card ]) p2rest
              | Player2 -> go p1rest (p2rest @ [ p2card; p1card ])
            in

            if List.length p1rest >= p1card && List.length p2rest >= p2card then
              let player1_sub = List.take p1rest p1card and player2_sub = List.take p2rest p2card in
              let { SimResult.winner; _ } = simulate2 player1_sub player2_sub in
              continue winner
            else if p1card > p2card then continue Player1
            else continue Player2
      in

      let result = go player1 player2 in
      (* printf "==> winner: %s\n\n"
         ( match result with
         | { SimResult.winner = Player1; _ } -> "P1"
         | { SimResult.winner = Player2; _ } -> "P2" ); *)
      HandPair.Table.set cache ~key:(player1, player2) ~data:result;
      result

let hand_score player =
  let fold_fun idx acc v = acc + ((idx + 1) * v) in
  player |> List.rev |> List.foldi ~init:0 ~f:fold_fun

let () =
  let lines = In_channel.input_lines Stdio.stdin in
  let try_to_int s = Option.try_with (fun () -> Int.of_string s) in
  let all_numbers = lines |> List.concat_map ~f:(fun line -> try_to_int line |> Option.to_list) in
  let hand_size = List.length all_numbers / 2 in
  assert (hand_size * 2 = List.length all_numbers);
  let player1 = List.take all_numbers hand_size in
  let player2 = List.drop all_numbers hand_size in

  let part1_result = simulate player1 player2 in
  let part1_winner_hand = SimResult.winner_hand part1_result in
  printf "part1, winner final: %s\n" (part1_winner_hand |> List.to_string ~f:Int.to_string);
  let part1_winner_score = hand_score part1_winner_hand in
  printf "part1, winner score: %d\n" part1_winner_score;

  let part2_result = simulate2 player1 player2 in
  let part2_winner_hand = SimResult.winner_hand part2_result in
  printf "part2, winner final: %s\n" (part2_winner_hand |> List.to_string ~f:Int.to_string);
  let part2_winner_score = hand_score part2_winner_hand in
  printf "part2, winner score: %d\n" part2_winner_score;

  ()
