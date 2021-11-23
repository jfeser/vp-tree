open Base

let swap arr i j =
  let tmp = arr.(j) in
  arr.(j) <- arr.(i);
  arr.(i) <- tmp

let partition ~compare arr left right pivot_idx =
  let pivot = arr.(pivot_idx) in
  swap arr pivot_idx right;
  let store_idx = ref left in
  for i = left to right - 1 do
    if compare arr.(i) pivot < 0 then (
      swap arr !store_idx i;
      Int.incr store_idx)
  done;
  swap arr right !store_idx;
  !store_idx

let rec select ~compare arr left right k =
  if left = right then arr.(left)
  else
    let pivot_idx = left + Random.int (right - left + 1) in
    let pivot_idx = partition ~compare arr left right pivot_idx in
    if k = pivot_idx then arr.(k)
    else if k < pivot_idx then select ~compare arr left (pivot_idx - 1) k
    else select ~compare arr (pivot_idx + 1) right k

let%test_unit "select" =
  for _ = 0 to 100 do
    let arr = Array.init (Random.int 1000) ~f:(fun _ -> Random.int 1000) in
    let arr' = Array.copy arr in
    Array.sort ~compare arr';

    let k = Random.int (Array.length arr) in
    let expect = arr'.(k) in
    let actual = select ~compare arr 0 (Array.length arr - 1) k in
    [%test_result: int] ~expect actual
  done
