(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

(* SIMD instructions for ARM64 *)

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module I = Arm64_ast.Instruction_name
module D = Arm64_simd_defs

type operation_class = Pure

module Rounding_mode = struct
  type t =
    | Current (* Default is Nearest *)
    | Neg_inf
    | Pos_inf
    | Zero
    | Nearest

  let instruction_suffix = function
    | Neg_inf -> "Neg_inf"
    | Pos_inf -> "Pos_inf"
    | Zero -> "Zero"
    | Current -> "Current"
    | Nearest -> "Nearest"

  let equal t1 t2 =
    match t1, t2 with
    | Current, Current
    | Neg_inf, Neg_inf
    | Pos_inf, Pos_inf
    | Zero, Zero
    | Nearest, Nearest ->
      true
    | (Current | Neg_inf | Pos_inf | Zero | Nearest), _ -> false
end

module Float_cond = struct
  type t = Arm64_ast.Instruction_name.Float_cond.t =
    | EQ
    | GT
    | LE
    | GE
    | LT
    | NE
    | CC
    | CS
    | LS
    | HI

  let to_string t =
    match t with
    | EQ -> "eq"
    | GT -> "gt"
    | LE -> "le"
    | GE -> "ge"
    | LT -> "lt"
    | NE -> "ne"
    | CC -> "cc"
    | CS -> "cs"
    | LS -> "ls"
    | HI -> "hi"

  let equal t1 t2 =
    match t1, t2 with
    | EQ, EQ
    | GT, GT
    | LE, LE
    | GE, GE
    | LT, LT
    | NE, NE
    | CC, CC
    | CS, CS
    | LS, LS
    | HI, HI ->
      true
    | (EQ | GT | LE | GE | LT | NE | CC | CS | LS | HI), _ -> false
end

module Cond = struct
  type t =
    | EQ
    | GE
    | GT
    | LE
    | LT

  let to_string t =
    match t with
    | EQ -> "eq"
    | GE -> "ge"
    | GT -> "ne"
    | LE -> "le"
    | LT -> "lt"

  let equal t1 t2 =
    match t1, t2 with
    | EQ, EQ | GE, GE | GT, GT | LE, LE | LT, LT -> true
    | (EQ | GE | GT | LE | LT), _ -> false
end

let emit_rounding_mode (rm : Simd.Rounding_mode.t) : I.Rounding_mode.t =
  match rm with
  | Neg_inf -> I.Rounding_mode.M
  | Pos_inf -> I.Rounding_mode.P
  | Zero -> I.Rounding_mode.Z
  | Current -> I.Rounding_mode.X
  | Nearest -> I.Rounding_mode.N

let emit_float_cond (cond : Simd.Float_cond.t) : I.Float_cond.t =
  match cond with
  | EQ -> EQ
  | GT -> GT
  | LE -> LE
  | GE -> GE
  | LT -> LT
  | NE -> NE
  | CC -> CC
  | CS -> CS
  | LS -> LS
  | HI -> HI

let emit_cond (cond : Simd.Cond.t) : I.Cond.t =
  match cond with EQ -> EQ | GT -> GT | GE -> GE | LE -> LE | LT -> LT

(** [Seq] represents intrinsics that are emitted as a sequence of Neon
    instructions, including single scalar Neon instructions that do
    not have a corresponding "v" C intrinsic in "arm_neon.h". *)
module Instr_seq = struct
  type id =
    (* [*_match_sse] are emitted as a sequence of instructions that matches
       amd64 semantics of the same intrinsic [caml_simd_float32_min/max],
       regardless of the value of [FPCR.AH]. *)
    | Scalar_min_f32_match_sse
    | Scalar_max_f32_match_sse
    | Scalar_min_f64_match_sse
    | Scalar_max_f64_match_sse
    (* [Fmin/Fmax] are emitted as the corresponding arm64 single
       instructions. *)
    | Scalar_fmin_f32
    | Scalar_fmax_f32
    | Scalar_fmin_f64
    | Scalar_fmax_f64

  let t = id Arm64_simd_instrs.instr

  (* helpers *)
  let make_binary_float32 id instr = D.make_binary id instr D.reg_float32

  let make_binary_float id instr = D.make_binary id instr D.reg_float

  let make_binary_int id instr = make_binary id instr D.reg_int

  let make_unary_float32 id instr = D.make_unary id instr D.reg_float32

  let make_unary_float id instr = D.make_unary id instr D.reg_float

  let make_unary_int id instr = D.make_unary id instr D.reg_int

  (* instructions *)

  let scalar_min_f32_match_sse =
    make_binary_float32 Scalar_min_f32_match_sse None

  let scalar_max_f32_match_sse =
    make_binary_float32 Scalar_max_f32_match_sse None

  let scalar_min_f64_match_sse = make_binary_float Scalar_min_f64_match_sse None

  let scalar_max_f64_match_sse = make_binary_float Scalar_max_f64_match_sse None

  let scalar_fmin_f32 = make_binary_float32 Scalar_fmin_f32 I.FMIN

  let scalar_fmax_f32 = make_binary_float32 Scalar_fmax_f32 I.FMAX

  let scalar_fmin_f64 = make_binary_float Scalar_fmin_f32 I.FMIN

  let scalar_fmax_f64 = make_binary_float Scalar_fmax_f32 I.FMAX

  let equal _ _ = Misc.fatal_error "arm64/simd: impelment equal for Seq.t"

  let mnemonic _ = Misc.fatal_error "arm64/simd: impelment mnemonic for Seq.t"

  let print ppf _ = Misc.fatal_error "arm64/simd: impelment print for Seq.t"
end
[@@inline always]

module Pseudo_instr = struct
  type t =
    | Instruction of Arm64_simd_instrs.instr
    | Sequence of Instr_seq.t

  let equal t1 t2 =
    match t1, t2 with
    | Instruction i0, Instruction i1 -> Amd64_simd_instrs.equal i0 i1
    | Sequence s0, Sequence s1 -> Instr_seq.equal s0 s1
    | (Instruction _ | Sequence _), _ -> false

  let print ppf t =
    match t with
    | Instruction instr -> fprintf ppf "%s" instr.mnemonic
    | Sequence seq -> fprintf ppf "[seq] %s" (Instr_seq.mnemonic seq)
end

module Imm = struct
  type t =
    | Imm of int
    | Mode of Rounding_mode.t
    | Float_cond of Float_cond.t
    | Cond of Cond.t
    | Lane of int
    | Lanes of
        { src : int;
          dst : int
        }

  let print ppf t =
    match t with
    | Imm i -> Format.fprintf ppf "%d" i
    | Mode rm -> Format.fprintf ppf "%s" (Rounding_mode.to_string rm)
    | Float_cond fc -> Format.fprintf ppf "%s" (Float_cond.to_string fc)
    | Cond c -> Format.fprintf ppf "%s" (Cond.to_string c)
    | Lane i -> Format.fprintf ppf "%d" i
    | Lanes { src; dst } -> Format.fprintf ppf "src_lane=%d dst_lane=%d" src dst

  let equal t1 t2 =
    match t1, t2 with
    | Imm i1, Imm i2 -> Int.equal i1 i2
    | Mode rm1, Mode rm2 -> Rounding_mode.equal rm1 rm2
    | Float_cond fc1, Float_cond fc2 -> Float_cond.equal fc1 fc2
    | Cond c1, Cond c2 -> Cond.equal c1 c2
    | Lane i1, Lane i2 -> Int.equal i1 i2
    | Lanes { src; dst }, Lanes { src = src'; dst = dst' } ->
      Int.equal src src' && Int.equal dst dst'
    | (Imm _ | Mode _ | Float_cond _ | Cond _ | Lane _ | Lanes _), _ -> false
end

(* CR gyorsh: clean up constructor arguments *)
type operation =
  { instr : Pseudo_instr.t;
    imm : Imm.t option (* immediate arguments *)
  }

let instruction instr imm = { instr = Pseudo_instr.Instruction instr; imm }

let sequence instr imm = { instr = Pseudo_instr.Sequence instr; imm }

let is_pure_operation _op = true

let class_of_operation _op = Pure

let print_operation printreg (op : operation) ppf regs =
  (* CR gyorsh: does not support memory operands (except stack operands). *)
  Format.fprintf ppf "%s %a %a"
    (Pseudo_instr.print op.instr)
    (Format.pp_print_option Imm.print)
    op.imm
    (Format.pp_print_seq ~pp_sep:Format.pp_print_space printreg)
    (regs |> Array.to_seq)

let equal_operation { instr = instr0; imm = imm0 }
    { instr = instr1; imm = imm1 } =
  Pseudo_instr.equal instr0 instr1 && Option.equal Imm.equal imm0 imm1

let class_of_operation _op = Pure

let operation_is_pure op = match class_of_operation op with Pure -> true
