open Base
open Vpt.For_testing.Quickselect

let%test_unit "select" =
  for _ = 0 to 100 do
    let arr = Array.init (Random.int 1000) ~f:(fun _ -> Random.float 1.0) in
    let arr' = Array.copy arr in
    Array.sort ~compare:[%compare: float] arr';

    let k = Random.int (Array.length arr) in
    let expect = arr'.(k) in
    let actual = select arr 0 (Array.length arr - 1) k in
    [%test_result: float] ~expect actual
  done
