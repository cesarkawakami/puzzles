open Core_kernel

module type Modulo = sig
  val p : int
end

module Mod (M : Modulo) = struct
  type t = N of int

  let p = M.p

  let ( + ) a b = match (a, b) with N a, N b -> N Int.((a + b) % p)

  let ( - ) a b = match (a, b) with N a, N b -> N Int.((((a - b) % p) + p) % p)

  let ( * ) a b = match (a, b) with N a, N b -> N Int.(a * b % p)

  let ( = ) a b = match (a, b) with N a, N b -> Int.(a = b)

  let of_num x = N Int.(((x % p) + p) % p)

  let to_num x = match x with N x -> x

  let zero = N 0

  let one = N 1

  let log base logmand =
    let rec loop exp power =
      if exp > p then None
      else if power = logmand then Some exp
      else loop Int.(exp + 1) (power * base)
    in
    loop 0 one

  let modpow_pos a b =
    match a with
    | N a ->
        let open Int in
        let rec loop acc a b =
          if b = 0 then N acc
          else
            let new_acc = if b % 2 = 0 then acc else acc * a % p in
            loop new_acc (a * a % p) (b asr 1)
        in
        loop 1 a b

  let inv a = modpow_pos a Int.(p - 1)

  let ( ** ) a b = if b >= 0 then modpow_pos a b else modpow_pos (inv a) Int.(-b)
end

module ModP1 = Mod (struct
  let p = 20201227
end)

let solve test_case a b =
  let base = ModP1.of_num 7 in
  let card_pub = ModP1.of_num a and door_pub = ModP1.of_num b in
  let card_exp = ModP1.log base card_pub |> Option.value_exn in
  let door_exp = ModP1.log base door_pub |> Option.value_exn in
  let enc_key = ModP1.((base ** card_exp) ** door_exp) in
  printf "%s: part1, card exp: %d\n" test_case card_exp;
  printf "%s: part1, door exp: %d\n" test_case door_exp;
  printf "%s: part1, encryption key: %d\n" test_case (enc_key |> ModP1.to_num)

let () =
  solve "SMALL" 5764801 17807724;
  solve "LARGE" 13316116 13651422
