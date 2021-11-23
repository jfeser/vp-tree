open Base

let get : float array -> _ = Array.unsafe_get

let set : float array -> _ = Array.unsafe_set

let[@inline] swap (arr : float array) i j =
  let tmp = get arr j in
  set arr j @@ get arr i;
  set arr i tmp

let[@inline] float_compare (x : float) (y : float) : int =
  if Float.(x < y) then -1 else if Float.(x > y) then 1 else 0

let partition (arr : float array) left right pivot_idx =
  let pivot = get arr pivot_idx in
  swap arr pivot_idx right;
  let store_idx = ref left in
  for i = left to right - 1 do
    if float_compare (get arr i) pivot < 0 then (
      swap arr !store_idx i;
      Int.incr store_idx)
  done;
  swap arr right !store_idx;
  !store_idx

let rec select (arr : float array) left right k =
  if left = right then get arr left
  else
    let pivot_idx = left + Random.int (right - left + 1) in
    let pivot_idx = partition arr left right pivot_idx in
    if k = pivot_idx then get arr k
    else if k < pivot_idx then select arr left (pivot_idx - 1) k
    else select arr (pivot_idx + 1) right k

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
