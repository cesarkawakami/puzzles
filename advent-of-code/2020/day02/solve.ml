open Core_kernel

exception Parsing_error of string

let just_char = Tyre.conv Char.of_string Char.to_string (Tyre.regex Re.alpha)

let password_line_re =
  Tyre.compile
    Tyre.(
      int
      <&> str "-" *> int
      <&> blanks *> just_char
      <&> str ":" *> blanks *> Tyre.regex (Re.rep1 Re.alpha))

let parse_password_line (line : string) =
  let parse_result = Tyre.exec password_line_re line in
  match parse_result with
  | Ok (((min, max), c), password) -> (min, max, c, password)
  | Error _ -> raise (Parsing_error "fail!")

let is_valid_password_line (line : string) =
  let min, max, needle, password = parse_password_line line in
  let needle_count = String.count password ~f:(fun c -> Char.equal c needle) in
  min <= needle_count && needle_count <= max

let is_valid_password_line_pt2 (line : string) =
  let pos1, pos2, needle, password = parse_password_line line in
  let ch1 = String.get password (pos1 - 1) in
  let ch2 = String.get password (pos2 - 1) in
  Bool.to_int (Char.equal needle ch1) + Bool.to_int (Char.equal needle ch2) = 1

let () =
  let lines = In_channel.input_lines Stdio.stdin in
  let accumulate (func : string -> bool) (acc : int) (line : string) =
    acc + Bool.to_int (func line)
  in
  let valid_password_count_pt1 =
    List.fold lines ~init:0 ~f:(accumulate is_valid_password_line)
  in
  let valid_password_count_pt2 =
    List.fold lines ~init:0 ~f:(accumulate is_valid_password_line_pt2)
  in
  printf "%d %d\n" valid_password_count_pt1 valid_password_count_pt2
