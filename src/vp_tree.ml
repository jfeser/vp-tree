open Base
module A = MyArray

let ( <. ) = Float.( < )

let ( <=. ) = Float.( <= )

let ( >. ) = Float.( > )

let ( >=. ) = Float.( >= )

let ( =. ) = Float.( = )

let print_s s = Caml.print_endline @@ Base.Sexp.to_string_hum s

(* Vantage-point tree implementation
   Cf. "Data structures and algorithms for nearest neighbor search
   in general metric spaces" by Peter N. Yianilos for details.
   http://citeseerx.ist.psu.edu/viewdoc/\
   download?doi=10.1.1.41.4193&rep=rep1&type=pdf *)

(* Functorial interface *)

module type Point = sig
  type t [@@deriving compare, hash, sexp]

  (* dist _must_ be a metric *)
  val dist : t -> t -> float
end

module Make (P : Point) = struct
  type node = {
    vp : P.t;
    lb_low : Base.float;
    lb_high : Base.float;
    middle : Base.float;
    rb_low : Base.float;
    rb_high : Base.float;
    left : t;
    right : t;
  }

  and t = Empty | Node of node [@@deriving sexp]

  let new_node vp lb_low lb_high middle rb_low rb_high left right =
    Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right }

  type open_itv = { lbound : float; rbound : float }

  let new_open_itv lbound rbound =
    assert (lbound <=. rbound);
    { lbound; rbound }

  let in_open_itv x { lbound; rbound } = x >. lbound && x <. rbound

  let itv_dont_overlap left right =
    let a = left.lbound in
    let b = left.rbound in
    let c = right.lbound in
    let d = right.rbound in
    (* [a..b] [c..d] OR [c..d] [a..b] *)
    b <. c || d <. a

  let itv_overlap left right = not (itv_dont_overlap left right)

  let square (x : float) : float = x *. x

  let median xs =
    let n = A.length xs in
    Quickselect.select (Array.copy xs) 0 (n - 1) (n / 2)

  let variance (mu : float) (xs : float array) : float =
    A.fold_left (fun acc x -> acc +. square (x -. mu)) 0.0 xs

  (* compute distance of point at index 'q_i' to all other points *)
  let distances (q_i : int) (points : P.t array) : float array =
    let n = A.length points in
    assert (n > 1);
    let res = A.make (n - 1) 0.0 in
    let j = ref 0 in
    let q = points.(q_i) in
    for i = 0 to n - 1 do
      if i <> q_i then (
        res.(!j) <- P.dist q points.(i);
        Int.incr j)
    done;
    res

  (* this is optimal (slowest tree construction; O(n^2));
     but fastest query time *)
  let select_best_vp (points : P.t array) =
    let n = A.length points in
    if n = 0 then assert false
    else if n = 1 then (points.(0), 0.0, [||])
    else
      let curr_vp = ref 0 in
      let curr_mu = ref 0.0 in
      let curr_spread = ref 0.0 in
      for i = 0 to n - 1 do
        (* could be faster using a distance cache? *)
        let dists = distances !curr_vp points in
        let mu = median dists in
        let spread = variance mu dists in
        if spread >. !curr_spread then (
          curr_vp := i;
          curr_mu := mu;
          curr_spread := spread)
      done;
      (points.(!curr_vp), !curr_mu, A.remove !curr_vp points)

  (* to replace select_best_vp when working with too many points *)
  let select_good_vp rng (sample_size : int) (points : P.t array) =
    let n = A.length points in
    if sample_size * sample_size >= n then select_best_vp points
    else
      let candidates = A.bootstrap_sample rng sample_size points in
      let curr_vp = ref 0 in
      let curr_mu = ref 0.0 in
      let curr_spread = ref 0.0 in
      A.iteri
        (fun i p_i ->
          let sample = A.bootstrap_sample rng sample_size points in
          let dists = A.map (P.dist p_i) sample in
          let mu = median dists in
          let spread = variance mu dists in
          if spread >. !curr_spread then (
            curr_vp := i;
            curr_mu := mu;
            curr_spread := spread))
        candidates;
      (* we need the true mu to balance the tree;
         not the one gotten from the sample! *)
      let dists = distances !curr_vp points in
      let mu = median dists in
      (points.(!curr_vp), mu, A.remove !curr_vp points)

  (* to replace select_good_vp when working with way too many points,
     or if you really need the fastest possible tree construction *)
  let select_rand_vp rng (points : P.t array) =
    let n = A.length points in
    assert (n > 0);
    let vp = Random.State.int rng n in
    let dists = distances vp points in
    let mu = median dists in
    (points.(vp), mu, A.remove vp points)

  let rec create' select_vp points =
    let n = A.length points in
    if n = 0 then Empty
    else if n = 1 then new_node points.(0) 0. 0. 0. 0. 0. Empty Empty
    else
      let vp, mu, others = select_vp points in
      let dists = A.map (fun p -> (P.dist vp p, p)) others in
      let lefties, righties = A.partition (fun (d, _p) -> d <. mu) dists in
      let ldists, lpoints = A.split lefties in
      let rdists, rpoints = A.split righties in
      let lb_low, lb_high = A.min_max_def ldists (0., 0.) in
      let rb_low, rb_high = A.min_max_def rdists (0., 0.) in
      let middle = (lb_high +. rb_low) *. 0.5 in
      new_node vp lb_low lb_high middle rb_low rb_high
        (create' select_vp lpoints)
        (create' select_vp rpoints)

  let create quality points =
    let select_vp =
      match quality with
      | `Optimal -> select_best_vp
      | `Good ssize -> select_good_vp (Random.State.make_self_init ()) ssize
      | `Random -> select_rand_vp (Random.State.make_self_init ())
    in
    create' select_vp (A.of_list points)

  let rec find_nearest acc query tree =
    match tree with
    | Empty -> acc
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } -> (
        let x = P.dist vp query in
        if x =. 0.0 then Some (x, vp) (* can't get nearer than that *)
        else
          let tau, acc' =
            match acc with
            | None -> (x, Some (x, vp))
            | Some (tau, best) ->
                if x <. tau then (x, Some (x, vp)) else (tau, Some (tau, best))
          in
          let il = new_open_itv (lb_low -. tau) (lb_high +. tau) in
          let ir = new_open_itv (rb_low -. tau) (rb_high +. tau) in
          let in_il = in_open_itv x il in
          let in_ir = in_open_itv x ir in
          if x <. middle then
            match (in_il, in_ir) with
            | false, false -> acc'
            | true, false -> find_nearest acc' query left
            | false, true -> find_nearest acc' query right
            | true, true -> (
                match find_nearest acc' query left with
                | None -> find_nearest acc' query right
                | Some (tau, best) -> (
                    match find_nearest acc' query right with
                    | None -> Some (tau, best)
                    | Some (tau', best') ->
                        if tau' <. tau then Some (tau', best')
                        else Some (tau, best)))
          else
            (* x >= middle *)
            match (in_ir, in_il) with
            | false, false -> acc'
            | true, false -> find_nearest acc' query right
            | false, true -> find_nearest acc' query left
            | true, true -> (
                match find_nearest acc' query right with
                | None -> find_nearest acc' query left
                | Some (tau, best) -> (
                    match find_nearest acc' query left with
                    | None -> Some (tau, best)
                    | Some (tau', best') ->
                        if tau' <. tau then Some (tau', best')
                        else Some (tau, best))))

  let nearest_neighbor query tree =
    Option.value_exn (find_nearest None query tree)

  let rec to_list_loop acc = function
    | Empty -> acc
    | Node n ->
        let acc' = to_list_loop acc n.right in
        to_list_loop (n.vp :: acc') n.left

  let to_list tree = to_list_loop [] tree

  let neighbors query tol tree =
    let rec loop acc = function
      | Empty -> acc
      | Node { vp; lb_low; lb_high; rb_low; rb_high; left; right; _ } ->
          (* should we include vp? *)
          let d = P.dist vp query in
          let acc' = if d <=. tol then vp :: acc else acc in
          let lbound = Float.max 0.0 (d -. tol) in
          let rbound = d +. tol in
          let itv = new_open_itv lbound rbound in
          (* should we inspect the left? *)
          let lmatches =
            let itv_left = new_open_itv lb_low lb_high in
            if itv_overlap itv itv_left then
              (* further calls to P.dist needed? *)
              if d +. lb_high <=. tol then
                (* all descendants are included *)
                to_list_loop acc' left
              else loop acc' left
            else acc'
          in
          (* should we inspect the right? *)
          let itv_right = new_open_itv rb_low rb_high in
          if itv_overlap itv itv_right then
            (* further calls to P.dist needed? *)
            if d +. rb_high <=. tol then to_list_loop lmatches right
            else loop lmatches right
          else lmatches
    in
    loop [] tree

  let is_empty = function Empty -> true | Node _ -> false

  let root = function Empty -> None | Node { vp; _ } -> Some vp

  (* test if the tree invariant holds.
     If it doesn't, then we are in trouble... *)
  let rec check = function
    | Empty -> true
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } ->
        let bounds_OK =
          0.0 <=. lb_low && lb_low <=. lb_high
          && (lb_high <. middle || 0.0 =. middle)
          && middle <=. rb_low && rb_low <=. rb_high
        in
        bounds_OK
        && List.for_all ~f:(fun p -> P.dist vp p <. middle) (to_list left)
        && List.for_all ~f:(fun p -> P.dist vp p >=. middle) (to_list right)
        && check left && check right

  exception Found of P.t

  let find query tree =
    let rec loop = function
      | Empty -> ()
      | Node { vp; middle; left; right; _ } ->
          let d = P.dist vp query in
          if d =. 0.0 then raise (Found vp)
          else if d <. middle then loop left
          else loop right
    in
    try
      loop tree;
      None
    with Found p -> Some p

  let mem query tree = Option.is_some @@ find query tree

  let children n =
    match (n.left, n.right) with
    | (Node _ as l), (Node _ as r) -> [ l; r ]
    | Empty, (Node _ as n) | (Node _ as n), Empty -> [ n ]
    | Empty, Empty -> []

  let rec traverse base score query refr =
    match (query, refr) with
    | Node qn, Node rn when Float.(score qn rn <> infinity) ->
        base qn.vp rn.vp;

        let qc = children qn and rc = children rn in
        if (not (List.is_empty qc)) && not (List.is_empty rc) then
          List.iter qc ~f:(fun qcn ->
              List.iter rc ~f:(fun rcn -> traverse base score qcn rcn))
        else if not (List.is_empty qc) then
          List.iter qc ~f:(fun qcn -> traverse base score qcn refr)
        else if not (List.is_empty rc) then
          List.iter rc ~f:(fun rcn -> traverse base score query rcn)
    | _ -> ()

  let range lower upper t t' ~f ~init =
    let module P2 = struct
      type t = P.t * P.t [@@deriving compare, hash, sexp]
    end in
    let neighbors = ref init and called = Hash_set.create (module P2) in

    let base p p' =
      let d = P.dist p p' in
      if (lower <=. d && d <=. upper) && not (Hash_set.mem called (p, p')) then
        neighbors := f !neighbors p p'
    in
    let score n n' =
      let _dmin =
        Float.max 0. @@ (P.dist n.vp n'.vp -. n.lb_high -. n'.lb_high)
      in
      1.0
      (* if Float.(lower <=. dmin && dmin <=. upper) then dmin else Float.infinity *)
    in
    traverse base score t t';
    !neighbors
end
