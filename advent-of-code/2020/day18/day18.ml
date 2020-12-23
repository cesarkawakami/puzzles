open Core_kernel
module Number = Int

type expression_t =
  | Null
  | Term of Number.t
  | Subexpression of expression_t
  | Addition of expression_t * expression_t
  | Multiplication of expression_t * expression_t
[@@deriving show]

type token_t = Term of Number.t | Plus | Times | OpenPar | ClosePar [@@deriving show]

type parse_mode_t = LeftAssociative | AdditionPrecedence [@@deriving show]

type operator_t = Addition | Multiplication [@@deriving show]

let int_push_left n d =
  let len = String.length (Number.to_string n) in
  Number.((d * (Number.of_int 10 ** len)) + n)

let rec lex (s : char list) : (token_t list, string) result =
  let open Result.Let_syntax in
  match s with
  | [] -> Ok []
  | ch :: rest -> (
      let%bind subresult = lex rest in

      match ch with
      | '(' -> Ok (OpenPar :: subresult)
      | ')' -> Ok (ClosePar :: subresult)
      | '+' -> Ok (Plus :: subresult)
      | '*' -> Ok (Times :: subresult)
      | ' ' -> Ok subresult
      | digit when String.mem "0123456789" digit -> (
          let digit = Number.of_string (String.of_char digit) in
          match subresult with
          | Term v :: rest -> Ok (Term (int_push_left v digit) :: rest)
          | rest -> Ok (Term digit :: rest) )
      | _ -> Error (sprintf "problem parsing: %s" (String.of_char_list s)) )

let show_token_list (token_list : token_t list) : string = List.to_string ~f:show_token_t token_list

let rec replace_null_counting (expr : expression_t) (new_expr : expression_t) : expression_t * int =
  match expr with
  | Null -> (new_expr, 1)
  | Term v -> (Term v, 0)
  | Subexpression subexpr ->
      let subexpr_result, subexpr_count = replace_null_counting subexpr new_expr in
      (Subexpression subexpr_result, subexpr_count)
  | Addition (left, right) ->
      let left_sub, left_count = replace_null_counting left new_expr in
      let right_sub, right_count = replace_null_counting right new_expr in
      (Addition (left_sub, right_sub), left_count + right_count)
  | Multiplication (left, right) ->
      let left_sub, left_count = replace_null_counting left new_expr in
      let right_sub, right_count = replace_null_counting right new_expr in
      (Multiplication (left_sub, right_sub), left_count + right_count)

let replace_one_null (expr : expression_t) (new_expr : expression_t) : (expression_t, string) result
    =
  let resulting_expr, count = replace_null_counting expr new_expr in
  if count = 1 then Ok resulting_expr
  else
    Error
      (sprintf "replacing %s in %s, found %d instead of 1" (show_expression_t new_expr)
         (show_expression_t expr) count)

let rec push_operation (expr : expression_t) (op : operator_t) (mode : parse_mode_t) : expression_t
    =
  match op with
  | Multiplication -> Multiplication (expr, Null)
  | Addition -> (
      match mode with
      | LeftAssociative -> Addition (expr, Null)
      | AdditionPrecedence -> (
          match expr with
          | Multiplication (left, right) -> Multiplication (left, push_operation right op mode)
          | _ -> Addition (expr, Null) ) )

let rec parse (expr : expression_t) (tokens : token_t list) (mode : parse_mode_t) :
    (expression_t * token_t list, string) result =
  let open Result.Let_syntax in
  match tokens with
  | [] -> Ok (expr, [])
  | Term v :: rest ->
      let%bind expr = replace_one_null expr (Term v) in
      parse expr rest mode
  | Plus :: rest -> parse (push_operation expr Addition mode) rest mode
  | Times :: rest -> parse (push_operation expr Multiplication mode) rest mode
  | ClosePar :: rest -> Ok (Subexpression expr, rest)
  | OpenPar :: rest ->
      let%bind subexpr, rest = parse Null rest mode in
      let%bind expr = replace_one_null expr subexpr in
      parse expr rest mode

let rec evaluate (expr : expression_t) (null_fail : bool) : (Number.t, string) result =
  let open Result.Let_syntax in
  match expr with
  | Null -> if null_fail then Error "evaluating null" else Ok Number.zero
  | Term v -> Ok v
  | Addition (left, right) ->
      let%bind left_result = evaluate left null_fail in
      let%bind right_result = evaluate right null_fail in
      Ok Number.(left_result + right_result)
  | Multiplication (left, right) ->
      let%bind left_result = evaluate left null_fail in
      let%bind right_result = evaluate right null_fail in
      Ok Number.(left_result * right_result)
  | Subexpression subexpr -> evaluate subexpr null_fail

let expression_from_line (line : string) (mode : parse_mode_t) : (expression_t, string) result =
  let open Result.Let_syntax in
  let%bind tokens = lex (String.to_list line) in
  let%bind expr, loose_tokens = parse Null tokens mode in
  if List.is_empty loose_tokens then Ok expr
  else Error (sprintf "found loose tokens: %s" (show_token_list tokens))

let () =
  let lines = In_channel.input_lines Stdio.stdin |> List.map ~f:String.strip in
  let _ =
    [ ("part1", LeftAssociative); ("part2", AdditionPrecedence) ]
    |> List.iter ~f:(fun (part_name, mode) ->
           let expressions =
             lines
             |> List.map ~f:(fun line -> expression_from_line line mode)
             |> Result.all |> Result.ok_or_failwith
           in
           let results =
             expressions
             |> List.map ~f:(fun expr -> evaluate expr true)
             |> Result.all |> Result.ok_or_failwith
           in

           let _ =
             List.zip_exn lines results
             |> List.iter ~f:(fun (line, result) ->
                    printf "%s, %-50s = %s\n" part_name line (Number.to_string result))
           in

           let sum_of_results = results |> List.sum (module Number) ~f:Fn.id in
           let _ = printf "%s, sum of results: %s\n" part_name (Number.to_string sum_of_results) in
           ())
  in

  ()
