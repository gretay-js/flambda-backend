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

type operation =
  | Round_f32 of Rounding_mode.t
  | Round_f64 of Rounding_mode.t
  | Round_f32x4 of Rounding_mode.t
  | Round_f32_i64
  (* [Min_scalar_f32/Max_scalar_f32] are emitted as a sequence of instructions
     that matches amd64 semantics of the same intrinsic
     [caml_simd_float32_min/max], regardless of the value of [FPCR.AH]. *)
  | Min_scalar_f32
  | Max_scalar_f32
  | Min_scalar_f64
  | Max_scalar_f64
  (* [Fmin/Fmax] are emitted as the corresponding arm64 single instructions. *)
  | Fmin_f32
  | Fmax_f32
  | Zip1_f32
  | Zip1q_f32
  | Zip1q_f64
  | Zip2q_f64
  | Addq_i64
  | Subq_i64
  | Addq_f32
  | Subq_f32
  | Mulq_f32
  | Divq_f32
  | Minq_f32
  | Maxq_f32
  | Recpeq_f32
  | Sqrtq_f32
  | Rsqrteq_f32
  | Cvtq_s32_f32
  | Cvtq_f32_s32
  | Cvt_f64_f32
  | Cvt_f32_f64
  | Cvt_f64_s32
  | Cvt_s32_f64
  | Paddq_f32
  | Cmp_f32 of Float_cond.t
  | Cmpz_f32 of Float_cond.t
  | Cmpz_s32 of Cond.t
  | Mvnq_s32
  | Orrq_s32
  | Andq_s32
  | Eorq_s32
  | Negq_s32
  | Getq_lane_s32 of { lane : int (* 0 <= lane <= 3 *) }

let print_name op =
  match op with
  | Round_f32 rm -> "Round_f32_" ^ Rounding_mode.instruction_suffix rm
  | Round_f64 rm -> "Round_f64_" ^ Rounding_mode.instruction_suffix rm
  | Round_f32x4 rm -> "Round_f32x4_" ^ Rounding_mode.instruction_suffix rm
  | Round_f32_i64 -> "Round_f32_i"
  | Zip1_f32 -> "Zip1_f32"
  | Zip1q_f32 -> "Zip1q_f32"
  | Zip1q_f64 -> "Zip1q_f64"
  | Zip2q_f64 -> "Zip2q_f64"
  | Fmin_f32 -> "Fmin_f32"
  | Fmax_f32 -> "Fmax_f32"
  | Min_scalar_f32 -> "Min_scalar_f32"
  | Max_scalar_f32 -> "Max_scalar_f32"
  | Min_scalar_f64 -> "Min_scalar_f64"
  | Max_scalar_f64 -> "Max_scalar_f64"
  | Addq_i64 -> "Addq_i64"
  | Subq_i64 -> "Subq_i64"
  | Addq_f32 -> "Addq_f32"
  | Subq_f32 -> "Subq_f32"
  | Mulq_f32 -> "Mulq_f64"
  | Divq_f32 -> "Divq_f64"
  | Minq_f32 -> "Minq_f64"
  | Maxq_f32 -> "Maxq_f64"
  | Recpeq_f32 -> "Recpeq_f64"
  | Sqrtq_f32 -> "Sqrtq_f64"
  | Rsqrteq_f32 -> "Rsqrtq_f64"
  | Cvtq_s32_f32 -> "Cvtq_s32_f32"
  | Cvtq_f32_s32 -> "Cvtq_f32_s32"
  | Cvt_f64_f32 -> "Cvt_f64_f32"
  | Cvt_f32_f64 -> "Cvt_f32_f64"
  | Cvt_f64_s32 -> "Cvt_f64_s32"
  | Cvt_s32_f64 -> "Cvt_s32_f64"
  | Paddq_f32 -> "Paddq_f64"
  | Cmp_f32 cond -> "Cmp_f32_" ^ Float_cond.to_string cond
  | Cmpz_f32 cond -> "Cmp_f32_" ^ Float_cond.to_string cond
  | Cmpz_s32 cond -> "Cmpz_s32_" ^ Cond.to_string cond
  | Mvnq_s32 -> "Mvnq_s32"
  | Orrq_s32 -> "Orrq_s32"
  | Andq_s32 -> "Andq_s32"
  | Eorq_s32 -> "Eorq_s32"
  | Negq_s32 -> "Negq_s32"
  | Getq_lane_s32 { lane } -> "Getq_lane_s32_" ^ Int.to_string lane

let print_operation printreg op ppf arg =
  (* CR gyorsh: does not support memory operands (except stack operands). *)
  Format.fprintf ppf "%s %a" (print_name op)
    (Format.pp_print_seq ~pp_sep:Format.pp_print_space printreg)
    (arg |> Array.to_seq)

let equal_operation op1 op2 =
  match op1, op2 with
  | Round_f32 mode, Round_f32 mode'
  | Round_f64 mode, Round_f64 mode'
  | Round_f32x4 mode, Round_f32x4 mode' ->
    Rounding_mode.equal mode mode'
  | Round_f32_i64, Round_f32_i64
  | Min_scalar_f32, Min_scalar_f32
  | Max_scalar_f32, Max_scalar_f32
  | Min_scalar_f64, Min_scalar_f64
  | Max_scalar_f64, Max_scalar_f64
  | Fmin_f32, Fmin_f32
  | Fmax_f32, Fmax_f32
  | Zip1_f32, Zip1_f32
  | Zip1q_f32, Zip1q_f32
  | Zip1q_f64, Zip1q_f64
  | Zip2q_f64, Zip2q_f64
  | Addq_i64, Addq_i64
  | Subq_i64, Subq_i64
  | Addq_f32, Addq_f32
  | Subq_f32, Subq_f32
  | Mulq_f32, Mulq_f32
  | Divq_f32, Divq_f32
  | Minq_f32, Minq_f32
  | Maxq_f32, Maxq_f32
  | Recpeq_f32, Recpeq_f32
  | Sqrtq_f32, Sqrtq_f32
  | Rsqrteq_f32, Rsqrteq_f32
  | Cvtq_s32_f32, Cvtq_s32_f32
  | Cvtq_f32_s32, Cvtq_f32_s32
  | Cvt_f64_f32, Cvt_f64_f32
  | Cvt_f32_f64, Cvt_f32_f64
  | Cvt_f64_s32, Cvt_f64_s32
  | Cvt_s32_f64, Cvt_s32_f64
  | Paddq_f32, Paddq_f32
  | Mvnq_s32, Mvnq_s32
  | Orrq_s32, Orrq_s32
  | Andq_s32, Andq_s32
  | Eorq_s32, Eorq_s32
  | Negq_s32, Negq_s32 ->
    true
  | Getq_lane_s32 { lane = l }, Getq_lane_s32 { lane = l' } -> Int.equal l l'
  | Cmp_f32 c, Cmp_f32 c' -> Float_cond.equal c c'
  | Cmpz_f32 c, Cmpz_f32 c' -> Float_cond.equal c c'
  | Cmpz_s32 c, Cmpz_s32 c' -> Cond.equal c c'
  | ( ( Round_f32 _ | Round_f64 _ | Round_f32x4 _ | Round_f32_i64
      | Min_scalar_f32 | Max_scalar_f32 | Min_scalar_f64 | Max_scalar_f64
      | Fmin_f32 | Fmax_f32 | Zip1_f32 | Zip1q_f32 | Zip1q_f64 | Zip2q_f64
      | Addq_i64 | Subq_i64 | Addq_f32 | Subq_f32 | Mulq_f32 | Divq_f32
      | Minq_f32 | Maxq_f32 | Recpeq_f32 | Sqrtq_f32 | Rsqrteq_f32
      | Cvtq_s32_f32 | Cvtq_f32_s32 | Cvt_f64_f32 | Cvt_f32_f64 | Cvt_f64_s32
      | Cvt_s32_f64 | Paddq_f32 | Cmp_f32 _ | Cmpz_f32 _ | Cmpz_s32 _ | Mvnq_s32
      | Orrq_s32 | Andq_s32 | Eorq_s32 | Negq_s32 | Getq_lane_s32 _ ),
      _ ) ->
    false

let class_of_operation op =
  match op with
  | Round_f32 _ | Round_f64 _ | Round_f32x4 _ | Round_f32_i64 | Min_scalar_f32
  | Max_scalar_f32 | Min_scalar_f64 | Max_scalar_f64 | Fmin_f32 | Fmax_f32
  | Zip1_f32 | Zip1q_f32 | Zip1q_f64 | Zip2q_f64 | Addq_i64 | Subq_i64
  | Addq_f32 | Subq_f32 | Mulq_f32 | Divq_f32 | Minq_f32 | Maxq_f32 | Recpeq_f32
  | Sqrtq_f32 | Rsqrteq_f32 | Cvtq_s32_f32 | Cvtq_f32_s32 | Cvt_f64_f32
  | Cvt_f32_f64 | Cvt_f64_s32 | Cvt_s32_f64 | Paddq_f32 | Cmp_f32 _ | Cmpz_f32 _
  | Cmpz_s32 _ | Mvnq_s32 | Orrq_s32 | Andq_s32 | Eorq_s32 | Negq_s32
  | Getq_lane_s32 _ ->
    Pure

let operation_is_pure op = match class_of_operation op with Pure -> true
