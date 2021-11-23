open Base

let print_s s = Caml.print_endline @@ Sexp.to_string_hum s

module P = struct
  module T = struct
    type t = float array [@@deriving compare, sexp]

    let hash_fold_t = Hash.Builtin.hash_fold_array_frozen [%hash_fold: float]

    let hash = Hash.of_fold hash_fold_t
  end

  include T
  include Comparator.Make (T)

  let dist p p' =
    Array.fold2_exn p p'
      ~f:(fun sum x x' -> sum +. ((x -. x') *. (x -. x')))
      ~init:0.
    |> Float.sqrt

  let%test "" = Float.(dist [| 1.0 |] [| 0.0 |] = 1.0)
end

open Vp_tree.Make (P)

let%test_unit "range" =
  let dim = 1 in
  let mk_points () =
    List.init 5 ~f:(fun _ -> Array.init dim ~f:(fun _ -> Random.float 1.0))
  in
  let query = mk_points () and refr = mk_points () in

  print_s [%message (query : P.t list) (refr : P.t list)];

  let query_t = create Random query and refr_t = create Random refr in
  print_s [%message (query_t : t) (refr_t : t)];

  let expect =
    List.fold query
      ~init:(Map.empty (module P))
      ~f:(fun ns p ->
        Map.set ns ~key:p
          ~data:
            (Set.of_list (module P)
            @@ List.filter refr ~f:(fun p' -> Float.(P.dist p p' < 0.2))))
  in

  (* print_s [%message (t : t) (t' : t)]; *)
  let result =
    range 0.0 0.2 query_t refr_t
      ~f:(fun ns p p' ->
        Map.update ns p ~f:(function
          | None -> Set.singleton (module P) p'
          | Some ps -> Set.add ps p'))
      ~init:(Map.empty (module P))
  in
  [%test_result: Set.M(P).t Map.M(P).t] ~expect result
