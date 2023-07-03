open Base

let one_rand_point_2D () = (Random.float 1.0, Random.float 1.0)
let square x = x *. x

module P = struct
  type t = float * float [@@deriving compare, hash, sexp]

  let dist (x0, y0) (x1, y1) =
    Float.sqrt (square (x0 -. x1) +. square (y0 -. y1))
end

let%test_module "create" =
  (module struct
    open Vpt

    let points = List.init 1000 ~f:(fun _ -> one_rand_point_2D ())
    let sorted_points = List.sort ~compare:[%compare: P.t] points

    let test_create quality =
      let t = create P.dist quality points in
      assert (check P.dist t);
      assert (
        [%compare.equal: P.t list] sorted_points
          (List.sort ~compare:[%compare: P.t] @@ Iter.to_list @@ iter t))

    let%test_unit "optimal" = test_create `Optimal
    let%test_unit "good" = test_create (`Good 50)
    let%test_unit "random" = test_create `Random
  end)
