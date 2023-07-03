open Base

let ( <. ) = Float.( < )
let ( <=. ) = Float.( <= )
let ( >. ) = Float.( > )
let ( >=. ) = Float.( >= )
let ( =. ) = Float.( = )

module A = struct
  include Array

  (* smaller array, without elt at index 'i' *)
  let remove i a =
    let n = length a in
    assert (i >= 0 && i < n);
    Array.init (n - 1) ~f:(fun i' -> if i' < i then a.(i') else a.(i' + 1))

  (* <=> BatArray.min_max with default value in case of empty array *)
  let min_max_def a def =
    let n = length a in
    if n = 0 then def
    else
      let mini = ref (unsafe_get a 0) in
      let maxi = ref (unsafe_get a 0) in
      for i = 1 to n - 1 do
        let x = unsafe_get a i in
        mini := Float.min x !mini;
        maxi := Float.max x !maxi
      done;
      (!mini, !maxi)

  (* get one bootstrap sample of 'size' using sampling with replacement *)
  let bootstrap_sample rng size a =
    let n = length a in
    assert (n > 0);
    assert (size < n);
    init size ~f:(fun _ -> unsafe_get a (Base.Random.State.int rng n))
end

let print_s s = Caml.print_endline @@ Base.Sexp.to_string_hum s

(* Vantage-point tree implementation
   Cf. "Data structures and algorithms for nearest neighbor search
   in general metric spaces" by Peter N. Yianilos for details.
   http://citeseerx.ist.psu.edu/viewdoc/\
   download?doi=10.1.1.41.4193&rep=rep1&type=pdf *)

type 'a node = {
  vp : 'a;
  lb_low : Base.float;
  lb_high : Base.float;
  middle : Base.float;
  rb_low : Base.float;
  rb_high : Base.float;
  left : 'a t;
  right : 'a t;
}

and 'a t = Empty | Leaf of 'a array | Node of 'a node [@@deriving sexp]

let rec length = function
  | Empty -> 0
  | Leaf l -> Array.length l
  | Node n -> length n.left + length n.right

let new_node vp lb_low lb_high middle rb_low rb_high left right =
  Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right }

let[@inline] in_open_itv l r x = x >. l && x <. r

let[@inline] itv_dont_overlap a b c d =
  (* [a..b] [c..d] OR [c..d] [a..b] *)
  b <. c || d <. a

let[@inline] itv_overlap l r l' r' = not (itv_dont_overlap l r l' r')
let[@inline] square x = x *. x

let median xs =
  let n = A.length xs in
  Quickselect.select (A.copy xs) 0 (n - 1) (n / 2)

let variance (mu : float) (xs : float array) : float =
  A.fold ~f:(fun acc x -> acc +. square (x -. mu)) ~init:0.0 xs

(* compute distance of point at index 'q_i' to all other points *)
let distances dist q_i points : float array =
  let n = A.length points in
  assert (n > 1);
  let res = A.create ~len:(n - 1) 0.0 in
  let j = ref 0 in
  let q = points.(q_i) in
  for i = 0 to n - 1 do
    if i <> q_i then (
      res.(!j) <- dist q points.(i);
      Int.incr j)
  done;
  res

(* this is optimal (slowest tree construction; O(n^2));
   but fastest query time *)
let select_best_vp dist points =
  let n = A.length points in
  if n = 0 then assert false
  else if n = 1 then (points.(0), 0.0, [||])
  else
    let curr_vp = ref 0 in
    let curr_mu = ref 0.0 in
    let curr_spread = ref 0.0 in
    for i = 0 to n - 1 do
      (* could be faster using a distance cache? *)
      let dists = distances dist !curr_vp points in
      let mu = median dists in
      let spread = variance mu dists in
      if spread >. !curr_spread then (
        curr_vp := i;
        curr_mu := mu;
        curr_spread := spread)
    done;
    (points.(!curr_vp), !curr_mu, A.remove !curr_vp points)

(* to replace select_best_vp when working with too many points *)
let select_good_vp dist rng (sample_size : int) points =
  let n = A.length points in
  if sample_size * sample_size >= n then select_best_vp dist points
  else
    let candidates = A.bootstrap_sample rng sample_size points in
    let curr_vp = ref 0 in
    let curr_mu = ref 0.0 in
    let curr_spread = ref 0.0 in
    A.iteri
      ~f:(fun i p_i ->
        let sample = A.bootstrap_sample rng sample_size points in
        let dists = A.map ~f:(dist p_i) sample in
        let mu = median dists in
        let spread = variance mu dists in
        if spread >. !curr_spread then (
          curr_vp := i;
          curr_mu := mu;
          curr_spread := spread))
      candidates;
    (* we need the true mu to balance the tree;
       not the one gotten from the sample! *)
    let dists = distances dist !curr_vp points in
    let mu = median dists in
    (points.(!curr_vp), mu, A.remove !curr_vp points)

(* to replace select_good_vp when working with way too many points,
   or if you really need the fastest possible tree construction *)
let select_rand_vp dist rng points =
  let n = A.length points in
  assert (n > 0);
  let vp = Random.State.int rng n in
  let dists = distances dist vp points in
  let mu = median dists in
  (points.(vp), mu, A.remove vp points)

let rec create' dist select_vp leaf_size points =
  let n = A.length points in
  if n = 0 then Empty
  else if n <= leaf_size then Leaf points
  else if n = 1 then new_node points.(0) 0. 0. 0. 0. 0. Empty Empty
  else
    let vp, mu, others = select_vp points in
    let dists = A.map ~f:(fun p -> (dist vp p, p)) others in
    let lefties, righties = A.partition_tf dists ~f:(fun (d, _) -> d <. mu) in
    let ldists, lpoints = A.unzip lefties in
    let rdists, rpoints = A.unzip righties in
    let lb_low, lb_high = A.min_max_def ldists (0., 0.) in
    let rb_low, rb_high = A.min_max_def rdists (0., 0.) in
    new_node vp lb_low lb_high mu rb_low rb_high
      (create' dist select_vp leaf_size lpoints)
      (create' dist select_vp leaf_size rpoints)

(** Insert internal vantage points into leaves *)
let fixup dist tree =
  let rec fixup dist points = function
    | Empty -> Leaf (Array.of_list points)
    | Leaf ps -> Leaf (Array.append ps @@ Array.of_list points)
    | Node n ->
        let points = List.map (n.vp :: points) ~f:(fun p -> (dist n.vp p, p)) in
        let left, right =
          List.partition_tf points ~f:(fun (d, _) -> d <. n.middle)
        in
        let ldists, lpoints = List.unzip left in
        let rdists, rpoints = List.unzip right in
        let lb_low, lb_high =
          List.fold ldists ~init:(n.lb_low, n.lb_high) ~f:(fun (l, h) d ->
              (Float.min l d, Float.max h d))
        in
        let rb_low, rb_high =
          List.fold rdists ~init:(n.rb_low, n.rb_high) ~f:(fun (l, h) d ->
              (Float.min l d, Float.max h d))
        in
        Node
          {
            n with
            lb_low;
            lb_high;
            rb_low;
            rb_high;
            left = fixup dist lpoints n.left;
            right = fixup dist rpoints n.right;
          }
  in
  fixup dist [] tree

let create ?(leaf_size = 1) ?(state = Random.State.default) dist quality points
    =
  assert (leaf_size >= 0);
  let select_vp =
    match quality with
    | `Optimal -> select_best_vp dist
    | `Good ssize -> select_good_vp dist state ssize
    | `Random -> select_rand_vp dist state
  in
  create' dist select_vp leaf_size (A.of_list points) |> fixup dist

let rec iter tree f =
  match tree with
  | Empty -> ()
  | Leaf ps -> Array.iter ~f ps
  | Node n ->
      iter n.left f;
      iter n.right f

let neighbors dist query tol tree f =
  let rec loop = function
    | Empty -> ()
    | Leaf l ->
        for i = 0 to Array.length l - 1 do
          let p = l.(i) in
          if dist p query <=. tol then f p
        done
    | Node { vp; lb_low; lb_high; rb_low; rb_high; left; right; _ } ->
        (* should we include vp? *)
        let d = dist vp query in

        let lbound = Float.max 0.0 (d -. tol) in
        let rbound = d +. tol in

        (* should we inspect the left? *)
        if itv_overlap lbound rbound lb_low lb_high then
          (* further calls to P.dist needed? *)
          if d +. lb_high <=. tol then
            (* all descendants are included *)
            iter left f
          else loop left;

        (* should we inspect the right? *)
        if itv_overlap lbound rbound rb_low rb_high then
          (* further calls to P.dist needed? *)
          if d +. rb_high <=. tol then iter right f else loop right
  in
  loop tree

(* test if the tree invariant holds.
   If it doesn't, then we are in trouble... *)
let rec check dist = function
  | Empty | Leaf _ -> true
  | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } ->
      0.0 <=. lb_low && lb_low <=. lb_high
      && (lb_high <. middle || 0.0 =. middle)
      && middle <=. rb_low && rb_low <=. rb_high
      && Iter.for_all (fun p -> dist vp p <. middle) (iter left)
      && Iter.for_all (fun p -> dist vp p >=. middle) (iter right)
      && check dist left && check dist right

let rec range dist lower upper t t' f =
  match (t, t') with
  | Empty, _ | _, Empty -> ()
  | Node n, Node n' ->
      let d = dist n.vp n'.vp in

      if d +. n.lb_high +. n'.lb_high <. upper -. lower then
        iter n.left (fun p -> iter n'.left (fun p' -> f (p, p')))
      else if d -. n.lb_high -. n'.lb_high <=. upper -. lower then
        range dist lower upper n.left n'.left f;

      if d -. n.lb_high -. n'.rb_high <. upper -. lower then
        range dist lower upper n.left n'.right f;

      if d -. n.rb_high -. n'.lb_high <. upper -. lower then
        range dist lower upper n.right n'.left f;

      if d -. n.rb_high -. n'.rb_high <. upper -. lower then
        range dist lower upper n.right n'.right f
  | (Leaf _ as t), Node n ->
      range dist lower upper t n.left f;
      range dist lower upper t n.right f
  | Node n, (Leaf _ as t) ->
      range dist lower upper n.left t f;
      range dist lower upper n.right t f
  | Leaf l, Leaf l' ->
      for i = 0 to Array.length l - 1 do
        for j = 0 to Array.length l' - 1 do
          let p = Array.unsafe_get l i and p' = Array.unsafe_get l' j in
          let d = dist p p' in
          if lower <=. d && d <=. upper then f (p, p')
        done
      done
