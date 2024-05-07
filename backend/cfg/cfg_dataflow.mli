[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon xclerc for xclerc: try and unify Forward_transfer/Backward_transfer. *)

module type Domain_S = sig
  (* The domain is a join-semilattice with a lowest element. To ensure
     termination additionally all ascending chains have to be bounded. *)
  type t

  (** Identity element of the [join] operation. From definition this is also the
      lowest element in the domain. *)
  val bot : t

  (** Join operator of the join-semilattice. This operation has be associative,
      commutative and idempotent. *)
  val join : t -> t -> t

  (** Operator defined as ([less_equal x y] iff [equal (join x y) y]). Is
      separate from [join] for efficiency. *)
  val less_equal : t -> t -> bool
end

module Dataflow_result : sig
  type ('a, 'e) t =
    | Ok of 'a
    | Aborted of 'a * 'e
    | Max_iterations_reached
end

module type S = sig
  type domain

  type error

  module Instr : Identifiable.S with type t = int

  type _ map =
    | Block : domain Label.Tbl.t map
    | Instr : domain Instr.Tbl.t map
    | Both : (domain Instr.Tbl.t * domain Label.Tbl.t) map



  (** Perform the dataflow analysis on the passed CFG, returning [OK _] if a fix-point has
      been reached or [Max_iterartions_reached] if there is still pending work after
      [max_iteration] have been executed or [Aborted _] otherwise.

      An iteration is the processing of one element from the working set.
      The default [max_iterations] is [max_int]).

      The nested result is a partial map from labels to the domain values at the start of
      the corresponding blocks or from instruction ids to the domain values before the
      instruction. The type of the result is determined by [map] argument.  If
      [Max_iterations_reached _] or [Abort _] is returned then the contents of the map is
      not guaranteed to be sound.

      The [init] value is the initial value of entry points. *)
  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:domain ->
    map:'a map ->
    unit ->
    ('a, error) Dataflow_result.t
end

module type Forward_transfer = sig
  type domain

  type error

  type image =
    { normal : domain;
      exceptional : domain
    }

  val basic : domain -> Cfg.basic Cfg.instruction -> (domain, error) result

  val terminator : domain -> Cfg.terminator Cfg.instruction ->
    (image, error) result

  val exception_ : domain -> (domain, error) result
end


module type Backward_transfer = sig
  type domain

  type error

  val basic : domain -> Cfg.basic Cfg.instruction -> (domain, error) result

  val terminator :
    domain ->
    exn:domain ->
    Cfg.terminator Cfg.instruction ->
    (domain, error) result

  val exception_ : domain -> (domain, error) result
end

module Forward (D : Domain_S) (T : Forward_transfer with type domain = D.t) :
  S with type domain = D.t and type error = T.error

module Backward (D : Domain_S) (T : Backward_transfer with type domain = D.t) :
  S with type domain = D.t and type error = T.error
