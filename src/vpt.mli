open Base
(** Functorial interface. *)

(** @open *)
module type Point = sig
  type t [@@deriving compare, hash, sexp]
  (** A point. *)

  val dist : t -> t -> float
  (** [dist] should be a distance function: symmetric, zero and
      the diagonal and verifying the triangular inequality.
      Be _very_ careful with the implementation of your metric
      (dist x x = 0.0, NaN is not a proper distance, etc). *)
end

(** Create a vantage point tree.
    @open
*)
module Make (P : Point) : sig
  type t [@@deriving sexp]
  (** A vantage point tree. *)

  val create :
    ?leaf_size:int ->
    ?state:Random.State.t ->
    [ `Optimal | `Good of int | `Random ] ->
    P.t list ->
    t
  (** [create quality points]
      create a vantage point tree of given quality containing all points.

      Tree construction takes more time with higher quality.
      Tree query time takes less time with higher tree quality.
      If you have 100k or more points, use a `Good or `Random tree. *)

  val check : t -> bool
  (** [check tree] test the tree invariant.
      Should always be true.
      If invariant doesn't hold, then this library has a bug
      (or your distance function is not a proper metric). *)

  val range : float -> float -> t -> t -> (P.t * P.t -> unit) -> unit
end
