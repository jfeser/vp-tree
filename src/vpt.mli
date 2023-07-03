open Base

type 'a node = private {
  vp : 'a;
  lb_low : float;
  lb_high : float;
  middle : float;
  rb_low : float;
  rb_high : float;
  left : 'a t;
  right : 'a t;
}
(** A vantage point tree. *)

and 'a t = private Empty | Leaf of 'a array | Node of 'a node
[@@deriving sexp]

(** [dist] should be a distance function: symmetric, zero and
      the diagonal and verifying the triangular inequality.
      Be _very_ careful with the implementation of your metric
      (dist x x = 0.0, NaN is not a proper distance, etc). *)

val create :
  ?leaf_size:int ->
  ?state:Random.State.t ->
  ('a -> 'a -> float) ->
  [ `Optimal | `Good of int | `Random ] ->
  'a list ->
  'a t
(** [create quality points]
      create a vantage point tree of given quality containing all points.

      Tree construction takes more time with higher quality.
      Tree query time takes less time with higher tree quality.
If you have 100k or more points, use a `Good or `Random tree. *)

val length : 'a t -> int
val iter : 'a t -> ('a -> unit) -> unit

val check : ('a -> 'a -> float) -> 'a t -> bool
(** [check tree] test the tree invariant.
      Should always be true.
      If invariant doesn't hold, then this library has a bug
      (or your distance function is not a proper metric). *)

val neighbors :
  ('a -> 'b -> float) -> 'b -> float -> 'a t -> ('a -> unit) -> unit

val range :
  ('a -> 'b -> float) ->
  float ->
  float ->
  'a t ->
  'b t ->
  ('a * 'b -> unit) ->
  unit

module For_testing : sig
  module Quickselect : sig
    val select : float array -> int -> int -> int -> float
  end
end
