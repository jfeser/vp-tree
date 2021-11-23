(** Functorial interface. *)

module type Point = sig
  type t [@@deriving compare, hash, sexp]
  (** A point. *)

  val dist : t -> t -> float
  (** [dist] should be a distance function: symmetric, zero and
      the diagonal and verifying the triangular inequality.
      Be _very_ careful with the implementation of your metric
      (dist x x = 0.0, NaN is not a proper distance, etc). *)
end

module Make (P : Point) : sig
  type t [@@deriving sexp]
  (** A vantage point tree. *)

  (** Quality of the constructed tree.
      Tree construction takes more time with higher quality.
      Tree query time takes less time with higher tree quality.
      If you have 100k or more points, use a Good or Random tree. *)
  type quality =
    | Optimal
    | Good of int
    (* sample size *)
    | Random

  val create : quality -> P.t list -> t
  (** [create quality points]
      create a vantage point tree of given quality containing all points. *)

  val nearest_neighbor : P.t -> t -> float * P.t
  (** [nearest_neighbor p vpt] return the distance along with the nearest
      neighbor to query point [p] in [vpt]. Warning: there may be several
      points at this distance from [p] in [vpt],
      but a single (arbitrary) one is returned.
      If you are not happy with that, use a point type that is
      deduplicated (i.e. a point that holds the info for all points with
      the same coordinates). *)

  val neighbors : P.t -> float -> t -> P.t list
  (** [neighbors p tol vpt] return all points in [vpt] within
      [tol] distance from query point [p].
      I.e. all points returned are within [(d <= tol)]
      distance from [p]. *)

  val to_list : t -> P.t list
  (** [to_list vpt] return the list of points inside the [vpt],
      in an unspecified order. *)

  val is_empty : t -> bool
  (** [is_empty vpt] test if [vpt] is empty. *)

  val find : P.t -> t -> P.t
  (** [find query tree] return the first point with distance to [query] = 0.0.
      @raise [Not_found] if no such element exists.
      Warning: there may be several
      points at this distance from [p] in [vpt],
      but a single (arbitrary) one is returned. *)

  val mem : P.t -> t -> bool
  (** [mem query tree] return true if [query] can be found in [tree];
      false otherwise. *)

  val root : t -> P.t
  (** [root tree] return the root point of the tree.
      @raise [Not_found] if [tree] is empty. *)

  val check : t -> bool
  (** [check tree] test the tree invariant.
      Should always be true.
      If invariant doesn't hold, then this library has a bug
      (or your distance function is not a proper metric). *)

  val range :
    float -> float -> t -> t -> f:('a -> P.t -> P.t -> 'a) -> init:'a -> 'a
end
