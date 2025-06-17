[@@@ocaml.warning "+a"]

(* The same physical register has different names in the assembly encoding of
   instructions. The name determines the type of data the instruction operates
   on. *)

(* Float/SIMD register description *)
module Neon_reg_name : sig
  module Vector : sig
    type t =
      | V8B
      | V16B
      | V4H
      | V8H
      | V2S
      | V4S
      | V1D
      | V2D
  end

  module Scalar : sig
    type t =
      | B
      | H
      | S
      | D
      | Q
  end

  module Lane : sig
    (** Support alternative representations. *)
    type t =
      | V of Vector.t * int
      | S of Scalar.t * int
  end

  type t =
    | Vector of Vector.t
    | Scalar of Scalar.t
    | Lane of Lane.t
end

(* General-purpose register description *)
module GP_reg_name : sig
  type t =
    | W
    | X
    | WZR
    | XZR
    | WSP
    | SP
end

(* Register representation *)
module Reg_name : sig
  type t =
    | GP of GP_reg_name.t
    | Neon of Neon_reg_name.t
end

(* CR sspies: rename Reg.t, since it conflicts with the registers of the linear
   IR. *)
module Reg : sig
  type t

  val create : Reg_name.t -> int -> t

  val reg_x : int -> t

  val reg_w : int -> t

  val reg_d : int -> t

  val reg_s : int -> t

  val reg_q : int -> t

  val reg_v2d : int -> t

  val wzr : t

  val xzr : t

  val wsp : t

  val sp : t
end
