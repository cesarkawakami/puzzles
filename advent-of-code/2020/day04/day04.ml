open Core_kernel
open Option.Monad_infix
module StringSet = Set.Make (String)

let field_name_from_field field =
  match String.split ~on:':' field with
  | [ field_name; _ ] -> field_name
  | _ -> failwithf "error parsing: %s" field ()

let is_valid_passport_pt1 (passport : string) =
  let fields = String.split_on_chars ~on:[ ' '; '\n'; '\r' ] passport in
  let fields = List.filter ~f:(fun s -> String.length s > 0) fields in
  let field_names =
    StringSet.of_list (List.map ~f:field_name_from_field fields)
  in
  let expected_field_names =
    StringSet.of_list [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
  in
  StringSet.is_subset expected_field_names ~of_:field_names

let is_valid_passport_pt2 (passport : string) =
  let is_valid_field field =
    try
      match String.split ~on:':' field with
      | [ "byr"; value ] ->
          let value = Int.of_string value in
          1920 <= value && value <= 2002
      | [ "iyr"; value ] ->
          let value = Int.of_string value in
          2010 <= value && value <= 2020
      | [ "eyr"; value ] ->
          let value = Int.of_string value in
          2020 <= value && value <= 2030
      | [ "hgt"; value ] -> (
          match
            Option.first_some
              ( String.chop_suffix value ~suffix:"cm" >>= fun value ->
                let value = Int.of_string value in
                Some (150 <= value && value <= 193) )
              ( String.chop_suffix value ~suffix:"in" >>= fun value ->
                let value = Int.of_string value in
                Some (59 <= value && value <= 76) )
          with
          | Some true -> true
          | _ -> false )
      | [ "hcl"; value ] ->
          Str.string_match
            (Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$")
            value 0
      | [ "ecl"; value ] -> (
          match
            List.find [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
              ~f:(fun v -> String.equal v value)
          with
          | Some _ -> true
          | None -> false )
      | [ "pid"; value ] ->
          Str.string_match
            (Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$")
            value 0
      | _ -> false
    with Failure _ -> false
  in
  let fields = String.split_on_chars ~on:[ ' '; '\n'; '\r' ] passport in
  let fields = List.filter ~f:is_valid_field fields in
  let field_names =
    StringSet.of_list (List.map ~f:field_name_from_field fields)
  in
  let expected_field_names =
    StringSet.of_list [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
  in
  StringSet.is_subset expected_field_names ~of_:field_names

let () =
  let data = In_channel.input_all Stdio.stdin in
  let passports = Str.split (Str.regexp_string "\n\n") data in
  let valid_passport_count_pt1 =
    List.fold passports ~init:0 ~f:(fun acc p ->
        acc + Bool.to_int (is_valid_passport_pt1 p))
  in
  let () = printf "part1, count: %d\n" valid_passport_count_pt1 in
  let valid_passport_count_pt2 =
    List.fold passports ~init:0 ~f:(fun acc p ->
        acc + Bool.to_int (is_valid_passport_pt2 p))
  in
  let () = printf "part2, count: %d\n" valid_passport_count_pt2 in
  ()
