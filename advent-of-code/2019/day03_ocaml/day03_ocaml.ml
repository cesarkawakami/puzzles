open Core
open Core_bench

let () =
  let tests = [
    Bench.Test.create ~name: "baseline" Day03_ocaml_baseline.benchtest;
    (* Bench.Test.create ~name: "set" Day03_ocaml_set.benchtest; *)
    Bench.Test.create ~name: "hashset" Day03_ocaml_hashset.benchtest;
    Bench.Test.create ~name: "hashset_opt" Day03_ocaml_hashset_opt.benchtest;
    Bench.Test.create ~name: "array" Day03_ocaml_array.benchtest;
  ]
  in
  Bench.make_command tests |> Command.run
