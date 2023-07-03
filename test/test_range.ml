open Base
open Vpt

let print_s s = Caml.print_endline @@ Sexp.to_string_hum s

module P = struct
  module T = struct
    type t = float array [@@deriving compare, sexp]

    let hash_fold_t = Hash.Builtin.hash_fold_array_frozen [%hash_fold: float]
    let hash = Hash.of_fold hash_fold_t
  end

  include T
  include Comparator.Make (T)

  let n_dist = ref 0

  let dist p p' =
    Int.incr n_dist;
    Array.fold2_exn p p'
      ~f:(fun sum x x' -> sum +. ((x -. x') *. (x -. x')))
      ~init:0.
    |> Float.sqrt

  let%test "" = Float.(dist [| 1.0 |] [| 0.0 |] = 1.0)
end

module P2 = struct
  module T = struct
    type t = P.t * P.t [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

let rec n_nodes = function
  | Node n -> 1 + n_nodes n.left + n_nodes n.right
  | _ -> 0

let%test_unit "range" =
  let dim = 1 in
  let n = 1000 in
  let mk_points () =
    List.init n ~f:(fun _ -> Array.init dim ~f:(fun _ -> Random.float 1.0))
  in
  let query = mk_points () and refr = mk_points () in
  let query_t = create P.dist ~leaf_size:32 `Optimal query
  and refr_t = create P.dist ~leaf_size:32 `Optimal refr in

  let expect =
    List.fold query
      ~init:(Set.empty (module P2))
      ~f:(fun ns p ->
        List.fold refr ~init:ns ~f:(fun ns p' ->
            let d = P.dist p p' in
            if Float.(0.0 <= d && d <= 0.2) then Set.add ns (p, p') else ns))
  in

  (* print_s [%message (t : t) (t' : t)]; *)
  let result =
    P.n_dist := 0;
    range P.dist 0.0 0.2 query_t refr_t
    |> Iter.to_list
    |> Set.of_list (module P2)
  in
  print_s [%message (!P.n_dist : int) (n * n : int)];
  let missing = Set.diff expect result and extra = Set.diff result expect in
  [%test_result: int] ~message:"result length" ~expect:(Set.length expect)
    (Set.length result);
  [%test_result: int] ~message:"num missing" ~expect:0 (Set.length missing);
  [%test_result: Set.M(P2).t] ~message:"missing"
    ~expect:(Set.empty (module P2))
    missing;
  [%test_result: Set.M(P2).t] ~expect:(Set.empty (module P2)) extra

(* let%test_unit "optimal-leaf-size" = *)
(*   let dim = 1 in *)
(*   let n = 1000 in *)
(*   let mk_points () = *)
(*     List.init n ~f:(fun _ -> Array.init dim ~f:(fun _ -> Random.float 1.0)) *)
(*   in *)
(*   let query = mk_points () and refr = mk_points () in *)
(*   let rec loop min_time best_size leaf_size = *)
(*     if leaf_size >= 256 then *)
(*       print_s [%message (min_time : float) (best_size : int)] *)
(*     else *)
(*       let query_t = create P.dist ~leaf_size `Random query *)
(*       and refr_t = create P.dist ~leaf_size `Random refr in *)

(*       let time = *)
(*         P.n_dist := 0; *)
(*         let start = Unix.gettimeofday () in *)
(*         ignore (range P.dist 0.0 0.2 query_t refr_t |> Iter.to_list : P2.t list); *)
(*         let end_ = Unix.gettimeofday () in *)
(*         end_ -. start *)
(*       in *)
(*       print_s [%message (time : float) (leaf_size : int)]; *)

(*       if Float.(time < min_time) then loop time leaf_size (leaf_size * 2) *)
(*       else loop min_time best_size (leaf_size * 2) *)
(*   in *)
(*   loop Float.infinity 1 1 *)
