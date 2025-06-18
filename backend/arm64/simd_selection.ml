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

open Simd
module S = Arm64_simd_instrs

(* SIMD instruction selection for ARM64 *)

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(* Intrinsics naming conventions:

   "caml_simd_*" for intrinsics used in the compiler distribution libraries, for
   example "caml_simd_float32_round_current" and "caml_simd_float32_min", or in
   compiler tests, for example "caml_simd_vec128_interleave_low_32". The
   behavior must match the corresponding amd64 intrinsics, and the name usually
   matches the corresponding "caml_sse*" intrinsic.

   "caml_neon_<type>_<mnemonic>" for example scalar type
   "caml_neon_float32_fmax" and vector type "caml_neon_float64x2_zip1" where the
   constructor such as [Zip1q_f64] matches the naming convention used by the
   standard arm intrinsics.

   Some intrinsics have both names to make it easier to correlate with both
   amd64 intrinsics and arm64 instructions, depending on context. *)

let instr instr ?i args = Some (Simd.instruction instr i, args)

let seq seq ?i args = Some (Simd.sequence seq i, args)

let two_args name args =
  match args with
  | [arg1; arg2] -> arg1, arg2
  | _ ->
    Misc.fatal_errorf "Cmm_builtins: expected exactly 2 arguments for %s" name

let swap_args args =
  match args with
  | [arg1; arg2] -> [arg2; arg1]
  | _ ->
    Misc.fatal_errorf "Simd_selection: expected exactly 2 arguments for %s" name

let select_simd_instr op args =
  match op with
  | "caml_simd_float32_round_neg_inf" -> instr S.vrndms_f32 args
  | "caml_simd_float32_round_pos_inf" -> instr S.vrndps_f32 args
  | "caml_simd_float32_round_towards_zero" -> instr S.vrndzs_f32 args
  | "caml_simd_float32_round_current" -> instr S.vrndis_f32 args
  | "caml_neon_float64_round_current" -> instr S.vrndi_f64 args
  | "caml_simd_float32_round_nearest" -> instr S.vrndns_f32 args
  | "caml_neon_float64_round_nearest" -> instr S.vrndn_f64 args
  | "caml_simd_cast_float32_int64" -> instr S.vcvtns_s64_f32 args
  | "caml_simd_float32_min" -> Some (Min_scalar_f32, args)
  | "caml_simd_float32_max" -> Some (Max_scalar_f32, args)
  | "caml_simd_float64_min" -> Some (Min_scalar_f64, args)
  | "caml_simd_float64_max" -> Some (Max_scalar_f64, args)
  | "caml_neon_float32_fmin" -> Some (Fmin_f32, args)
  | "caml_neon_float32_fmax" -> Some (Fmax_f32, args)
  | "caml_neon_float32x2_zip1" -> instr S.Vzip1_f32 args
  | "caml_simd_vec128_interleave_low_32" | "caml_neon_float32x4_zip1" ->
    instr S.vzip1q_f32 args
  | "caml_simd_vec128_interleave_low_64" | "caml_neon_float64x2_zip1" ->
    instr S.vzip1q_f64 args
  | "caml_simd_vec128_interleave_high_64" | "caml_neon_float64x2_zip2" ->
    instr S.vzip2q_f64 args
  | "caml_simd_int64x2_add" | "caml_neon_int64x2_add" -> instr S.vaddq_s64 args
  | "caml_simd_int64x2_sub" | "caml_neon_int64x2_sub" -> instr S.vsubq_s64 args
  | "caml_neon_float32x4_add" -> instr S.vaddq_f32 args
  | "caml_neon_float32x4_sub" -> instr S.vsubq_f32 args
  | "caml_neon_float32x4_mul" -> instr S.vmulq_f32 args
  | "caml_neon_float32x4_div" -> instr S.vdivq_f32 args
  | "caml_neon_float32x4_min" -> instr S.vminq_f32 args
  | "caml_neon_float32x4_max" -> instr S.vmaxq_f32 args
  | "caml_neon_float32x4_rcp" -> instr S.vrecpeq_f32 args
  | "caml_neon_float32x4_sqrt" -> insr vsqrtq_f32 args
  | "caml_neon_float32x4_rsqrt" -> insr vrsqrteq_f32 args
  | "caml_neon_float32x4_round_current" -> instr S.vrndiq_f32 args
  | "caml_neon_float32x4_round_nearest" -> instr S.vrndnq_f32 args
  | "caml_neon_float32x4_to_int32x4" -> instr S.vcvtq_s32_f32 args
  | "caml_neon_float32x2_to_float64x2" -> instr S.vcvt_f64_f32 args
  | "caml_neon_float32x4_hadd" -> instr S.vpaddq_f32 args
  | "caml_neon_float32x4_cmeq" -> instr S.vceqq_f32 args
  | "caml_neon_float32x4_cmgt" -> instr S.vcgtq_f32 args
  | "caml_neon_float32x4_cmge" -> instr S.vcgeq_f32 args
  | "caml_neon_float32x4_cmle" ->
    (* FCMLE is only supported with ZERO. *)
    let args = swap_args name args in
    instr S.vcgeq_f32 args
  | "caml_neon_float32x4_cmlt" ->
    (* FCMLT is only supported with ZERO. *)
    let args = swap_args name args in
    instr S.vcgtq_f32 args
  | "caml_neon_int32x4_cmpeqz" -> instr S.vceqzq_f32 args
  | "caml_neon_int32x4_cmpgez" -> instr S.vcgezq_f32 args
  | "caml_neon_int32x4_cmpgtz" -> instr S.vcgtzq_f32 args
  | "caml_neon_int32x4_cmplez" -> instr S.vclezq_f32 args
  | "caml_neon_int32x4_cmpltz" -> instr S.vcltzq_f32 args
  | _ -> None

let select_operation_cfg op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Operation.Specific (Isimd op), args)

let pseudoregs_for_operation _ arg res = arg, res

(* See `amd64/simd_selection.ml`. *)

let vector_width_in_bits = 128

let vectorize_operation _ ~arg_count:_ ~res_count:_ ~alignment_in_bytes:_
    (_ : Operation.t list) :
    Vectorize_utils.Vectorized_instruction.t list option =
  None
