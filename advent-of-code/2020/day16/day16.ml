open Core_kernel

type range_t = int * int

type field_spec_t = string * range_t list

let your_ticket_separator = "your ticket:"

let nearby_ticket_separator = "nearby tickets:"

let parse_range (range_desc : string) : range_t =
  match range_desc |> String.split ~on:'-' with
  | [ start_str; end_str ] ->
      let range_start = Int.of_string start_str in
      let range_end = Int.of_string end_str in
      (range_start, range_end)
  | _ -> failwithf "unable to parse range: %s" range_desc ()

let parse_field_spec (line : string) : field_spec_t =
  match line |> String.split ~on:':' with
  | [ name; ranges_desc ] ->
      let ranges =
        ranges_desc
        |> Re2.split (Re2.create_exn "or")
        |> List.map ~f:String.strip |> List.map ~f:parse_range
      in
      (name, ranges)
  | _ -> failwithf "unable to parse field spec: %s" line ()

let parse_ticket (line : string) : int list =
  line |> String.split ~on:',' |> List.map ~f:Int.of_string

let parse_input (lines : string list) : field_spec_t list * int list * int list list =
  let lines =
    lines |> List.map ~f:String.strip |> List.filter ~f:(fun line -> not (String.is_empty line))
  in
  let field_spec_lines, rest =
    lines |> List.split_while ~f:(fun line -> not (String.equal line your_ticket_separator))
  in
  let your_ticket_lines, rest =
    rest
    |> List.drop_while ~f:(fun line -> String.equal line your_ticket_separator)
    |> List.split_while ~f:(fun line -> not (String.equal line nearby_ticket_separator))
  in
  let nearby_ticket_lines =
    rest |> List.drop_while ~f:(fun line -> String.equal line nearby_ticket_separator)
  in
  let field_specs = field_spec_lines |> List.map ~f:parse_field_spec in
  let my_ticket =
    match your_ticket_lines with
    | [ my_ticket_line ] -> parse_ticket my_ticket_line
    | _ -> failwithf "unable to parse my ticket: %s" (BatPervasives.dump your_ticket_lines) ()
  in
  let nearby_tickets = nearby_ticket_lines |> List.map ~f:parse_ticket in
  (field_specs, my_ticket, nearby_tickets)

let part1_get_invalid_values (field_specs : field_spec_t list) (nearby_tickets : int list list) :
    int list =
  let ticket_values = nearby_tickets |> List.join in
  let all_ranges = field_specs |> List.map ~f:(fun (_, ranges) -> ranges) |> List.join in
  let value_in_some_range value =
    List.exists all_ranges ~f:(fun (range_start, range_end) ->
        range_start <= value && value <= range_end)
  in
  let invalid_values =
    ticket_values |> List.filter ~f:(fun value -> not (value_in_some_range value))
  in
  invalid_values

let is_valid_ticket (field_specs : field_spec_t list) (ticket : int list) : bool =
  let all_ranges = field_specs |> List.map ~f:(fun (_, ranges) -> ranges) |> List.join in
  ticket
  |> List.for_all ~f:(fun value ->
         List.exists all_ranges ~f:(fun (range_start, range_end) ->
             range_start <= value && value <= range_end))

let is_combination_compatible (field_spec : field_spec_t) (field_number : int)
    (tickets : int list list) : bool =
  let _, ranges = field_spec in
  List.for_all tickets ~f:(fun ticket ->
      let value = List.nth_exn ticket field_number in
      List.exists ranges ~f:(fun (range_start, range_end) ->
          range_start <= value && value <= range_end))

let compatible_field_numbers (field_spec : field_spec_t) (tickets : int list list) : int list =
  let candidates = List.range 0 (List.length (List.hd_exn tickets)) in
  let compatible_candidates =
    candidates |> List.filter ~f:(fun cand -> is_combination_compatible field_spec cand tickets)
  in
  compatible_candidates

let rec naive_sat_solve (field_specs : field_spec_t list) (candidate_lists : int list String.Map.t)
    (known_mappings : int String.Map.t) : int String.Map.t list =
  if String.Map.length known_mappings = List.length field_specs then [ known_mappings ]
  else
    let assigned_field_numbers =
      known_mappings |> String.Map.to_alist |> List.map ~f:(fun (_, v) -> v)
    in
    let field_number_has_been_assigned v = List.exists assigned_field_numbers ~f:(fun w -> v = w) in
    let candidates_for field_name =
      String.Map.find_exn candidate_lists field_name
      |> List.filter ~f:(Fn.non field_number_has_been_assigned)
    in
    let current_field_name, candidates =
      field_specs
      |> List.map ~f:(fun (name, _) -> (name, candidates_for name))
      |> List.filter ~f:(fun (name, _) -> not (String.Map.mem known_mappings name))
      |> List.min_elt ~compare:(fun (_, cands1) (_, cands2) ->
             Int.compare (List.length cands1) (List.length cands2))
      |> fun x -> match x with Some v -> v | None -> failwith "???"
    in
    let _ =
      printf "assigned: %s\n"
        ( known_mappings |> String.Map.to_alist
        |> List.map ~f:(fun (k, v) -> sprintf "%s -> %d" k v)
        |> String.concat ~sep:", " )
    in
    let _ =
      printf "  selected: %s, candidates: %s\n" current_field_name (BatPervasives.dump candidates)
    in
    let subresults =
      candidates
      |> List.map ~f:(fun candidate ->
             let new_mappings =
               String.Map.add_exn known_mappings ~key:current_field_name ~data:candidate
             in
             naive_sat_solve field_specs candidate_lists new_mappings)
    in
    List.join subresults

let solve_field_numbers (field_specs : field_spec_t list) (tickets : int list list) : int list =
  let candidate_lists =
    field_specs
    |> List.fold ~init:String.Map.empty ~f:(fun acc ((field_name, _) as field_spec) ->
           let candidates = compatible_field_numbers field_spec tickets in
           String.Map.add_exn acc ~key:field_name ~data:candidates)
  in
  let _ =
    candidate_lists
    |> String.Map.iteri ~f:(fun ~key ~data ->
           printf "cands for %s: %s\n" key (BatPervasives.dump data))
  in
  let _ = Out_channel.flush Stdio.stdout in
  (* let _ = printf "candidate lists:\n%s\n" (BatPervasives.dump candidate_lists) in *)
  match naive_sat_solve field_specs candidate_lists String.Map.empty with
  | [ final_mapping ] ->
      field_specs
      |> List.map ~f:(fun (field_name, _) -> String.Map.find_exn final_mapping field_name)
  | [] -> failwithf "found no solution!" ()
  | mappings -> failwithf "found too many solutions: %s" (BatPervasives.dump mappings) ()

let _ =
  let lines = In_channel.input_lines Stdio.stdin in
  let lines = lines |> List.map ~f:String.strip in
  let field_specs, my_ticket, nearby_tickets = parse_input lines in

  let part1_invalid_values = part1_get_invalid_values field_specs nearby_tickets in
  let _ = printf "part1, invalid values: %s\n" (BatPervasives.dump part1_invalid_values) in
  let _ = printf "part1, answer: %d\n" (part1_invalid_values |> List.sum (module Int) ~f:Fn.id) in

  let part2_desired_prefix = "departure" in
  (* let part2_desired_prefix = "class" in *)
  let valid_nearby_tickets =
    nearby_tickets |> List.filter ~f:(fun ticket -> is_valid_ticket field_specs ticket)
  in
  (* let field_numbers =
       field_specs
       |> List.map ~f:(fun field_spec -> infer_field_number field_spec valid_nearby_tickets)
     in *)
  let field_numbers = solve_field_numbers field_specs valid_nearby_tickets in
  let field_numbers_for_departure =
    List.zip_exn field_specs field_numbers
    |> List.filter ~f:(fun ((field_name, _), _) ->
           String.is_prefix field_name ~prefix:part2_desired_prefix)
    |> List.map ~f:(fun (_, field_number) -> field_number)
  in
  let part2_values =
    field_numbers_for_departure
    |> List.map ~f:(fun field_number -> List.nth_exn my_ticket field_number)
  in
  let part2_answer =
    part2_values |> List.fold ~init:Int63.one ~f:(fun acc v -> Int63.(acc * of_int v))
  in
  let _ = printf "part2, field numbers: %s\n" (BatPervasives.dump field_numbers) in
  let _ = printf "part2, values: %s\n" (BatPervasives.dump part2_values) in
  let _ = printf "part2, answer: %s\n" (Int63.to_string part2_answer) in

  ()
