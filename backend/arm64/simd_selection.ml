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

(* SIMD instruction selection for ARM64 *)

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(* CR-soon gyorsh: duplication of amd64 simd_selection check and erro
   reporting. *)
type error = Bad_immediate of string

exception Error of error

let bad_immediate fmt =
  Format.kasprintf (fun msg -> raise (Error (Bad_immediate msg))) fmt

(* Assumes untagged int *)
let[@ocaml.warning "-4"] extract_constant args name ~max =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < 0 || i > max
    then
      bad_immediate "Immediate for %s must be in range [0,%d] (got %d)" name max
        i;
    i, args
  | _ -> bad_immediate "Did not get integer immediate for %s" name

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

let select_simd_instr op args =
  match op with
  | "caml_simd_float32_round_neg_inf" -> Some (Round_f32 Neg_inf, args)
  | "caml_simd_float32_round_pos_inf" -> Some (Round_f32 Pos_inf, args)
  | "caml_simd_float32_round_towards_zero" -> Some (Round_f32 Zero, args)
  | "caml_simd_float32_round_current" -> Some (Round_f32 Current, args)
  | "caml_neon_float64_round_current" -> Some (Round_f64 Current, args)
  | "caml_simd_float32_round_near" -> Some (Round_f32 Nearest, args)
  | "caml_neon_float64_round_near" -> Some (Round_f64 Nearest, args)
  | "caml_simd_cast_float32_int64" -> Some (Round_f32_s64, args)
  (* min/max that match amd64 behavior, regardless of the value of FPCR.AH.
     implemented as a sequence of instructions *)
  | "caml_simd_float32_min" -> Some (Min_scalar_f32, args)
  | "caml_simd_float32_max" -> Some (Max_scalar_f32, args)
  | "caml_simd_float64_min" -> Some (Min_scalar_f64, args)
  | "caml_simd_float64_max" -> Some (Max_scalar_f64, args)
  (* min/max implemented as a single instruction. If FPCR.AH=1, matches amd64
     behavior. *)
  | "caml_neon_float32_min" -> Some (Fmin_f32, args)
  | "caml_neon_float32_max" -> Some (Fmax_f32, args)
  | "caml_neon_float64_min" -> Some (Fmin_f64, args)
  | "caml_neon_float64_max" -> Some (Fmax_f64, args)
  | "caml_neon_float32x2_zip1" -> Some (Zip1_f32, args)
  | "caml_simd_vec128_interleave_low_32" | "caml_neon_float32x4_zip1" ->
    Some (Zip1q_f32, args)
  | "caml_simd_vec128_interleave_low_64" | "caml_neon_float64x2_zip1" ->
    Some (Zip1q_f64, args)
  | "caml_simd_vec128_interleave_high_64" | "caml_neon_float64x2_zip2" ->
    Some (Zip2q_f64, args)
  | "caml_simd_int64x2_add" | "caml_neon_int64x2_add" -> Some (Addq_s64, args)
  | "caml_simd_int64x2_sub" | "caml_neon_int64x2_sub" -> Some (Subq_s64, args)
  | "caml_neon_int32x4_add" -> Some (Addq_s32, args)
  | "caml_neon_int32x4_hadd" -> Some (Paddq_s32, args)
  | "caml_neon_int64x2_hadd" -> Some (Paddq_s64, args)
  | "caml_neon_int32x4_sub" -> Some (Subq_s32, args)
  | "caml_neon_int32x4_abs" -> Some (Absq_s32, args)
  | "caml_neon_int64x2_abs" -> Some (Absq_s64, args)
  | "caml_neon_int32x4_max" -> Some (Maxq_s32, args)
  | "caml_neon_int32x4_min" -> Some (Minq_s32, args)
  | "caml_neon_int32x4_max_unsigned" -> Some (Maxq_u32, args)
  | "caml_neon_int32x4_min_unsigned" -> Some (Minq_u32, args)
  | "caml_neon_float32x4_add" -> Some (Addq_f32, args)
  | "caml_neon_float32x4_sub" -> Some (Subq_f32, args)
  | "caml_neon_float32x4_mul" -> Some (Mulq_f32, args)
  | "caml_neon_float32x4_div" -> Some (Divq_f32, args)
  | "caml_neon_float64x2_add" -> Some (Addq_f64, args)
  | "caml_neon_float64x2_sub" -> Some (Subq_f64, args)
  | "caml_neon_float64x2_mul" -> Some (Mulq_f64, args)
  | "caml_neon_float64x2_div" -> Some (Divq_f64, args)
  | "caml_neon_float32x4_min" -> Some (Minq_f32, args)
  | "caml_neon_float32x4_max" -> Some (Maxq_f32, args)
  | "caml_neon_float64x2_min" -> Some (Minq_f64, args)
  | "caml_neon_float64x2_max" -> Some (Maxq_f64, args)
  | "caml_neon_float32x4_rcp" -> Some (Recpeq_f32, args)
  | "caml_neon_float32x4_sqrt" -> Some (Sqrtq_f32, args)
  | "caml_neon_float32x4_rsqrt" -> Some (Rsqrteq_f32, args)
  | "caml_neon_float64x2_sqrt" -> Some (Sqrtq_f64, args)
  | "caml_neon_float64x2_rsqrt" -> Some (Rsqrteq_f64, args)
  | "caml_neon_float32x4_round_current" -> Some (Round_f32x4 Current, args)
  | "caml_neon_float32x4_round_near" -> Some (Round_f32x4 Nearest, args)
  | "caml_neon_cvt_int32x4_to_float32x4" -> Some (Cvtq_f32_s32, args)
  | "caml_neon_cvt_float32x4_to_int32x4" -> Some (Cvtq_s32_f32, args)
  | "caml_neon_cvt_float32x2_to_float64x2" -> Some (Cvt_f64_f32, args)
  | "caml_neon_cvt_float64x2_to_float32x2" -> Some (Cvt_f32_f64, args)
  | "caml_neon_cvt_int32x2_to_float64x2" -> Some (Cvt_f64_s32, args)
  | "caml_neon_cvt_float64x2_to_int32x2" -> Some (Cvt_s32_f64, args)
  | "caml_neon_float32x4_hadd" -> Some (Paddq_f32, args)
  | "caml_neon_float64x2_hadd" -> Some (Paddq_f64, args)
  | "caml_neon_float32x4_cmeq" -> Some (Cmp_f32 EQ, args)
  | "caml_neon_float32x4_cmge" -> Some (Cmp_f32 GE, args)
  | "caml_neon_float32x4_cmgt" -> Some (Cmp_f32 GT, args)
  | "caml_neon_float32x4_cmle" -> Some (Cmp_f32 LE, args)
  | "caml_neon_float32x4_cmlt" -> Some (Cmp_f32 LT, args)
  | "caml_neon_float64x2_cmeq" -> Some (Cmp_f64 EQ, args)
  | "caml_neon_float64x2_cmge" -> Some (Cmp_f64 GE, args)
  | "caml_neon_float64x2_cmgt" -> Some (Cmp_f64 GT, args)
  | "caml_neon_float64x2_cmle" -> Some (Cmp_f64 LE, args)
  | "caml_neon_float64x2_cmlt" -> Some (Cmp_f64 LT, args)
  | "caml_neon_int32x4_cmpeqz" -> Some (Cmpz_s32 EQ, args)
  | "caml_neon_int32x4_cmpgez" -> Some (Cmpz_s32 GE, args)
  | "caml_neon_int32x4_cmpgtz" -> Some (Cmpz_s32 GT, args)
  | "caml_neon_int32x4_cmplez" -> Some (Cmpz_s32 LE, args)
  | "caml_neon_int32x4_cmpltz" -> Some (Cmpz_s32 LT, args)
  | "caml_neon_int64x2_cmpeqz" -> Some (Cmpz_s64 EQ, args)
  | "caml_neon_int64x2_cmpgez" -> Some (Cmpz_s64 GE, args)
  | "caml_neon_int64x2_cmpgtz" -> Some (Cmpz_s64 GT, args)
  | "caml_neon_int64x2_cmplez" -> Some (Cmpz_s64 LE, args)
  | "caml_neon_int64x2_cmpltz" -> Some (Cmpz_s64 LT, args)
  | "caml_neon_int32x4_bitwise_not" -> Some (Mvnq_s32, args)
  | "caml_neon_int32x4_bitwise_or" -> Some (Orrq_s32, args)
  | "caml_neon_int32x4_bitwise_and" -> Some (Andq_s32, args)
  | "caml_neon_int32x4_bitwise_xor" -> Some (Eorq_s32, args)
  | "caml_neon_int32x4_neg" -> Some (Negq_s32, args)
  | "caml_neon_int32x4_cnt" -> Some (Cntq_s32, args)
  | "caml_neon_int64x2_bitwise_not" -> Some (Mvnq_s64, args)
  | "caml_neon_int64x2_bitwise_or" -> Some (Orrq_s64, args)
  | "caml_neon_int64x2_bitwise_and" -> Some (Andq_s64, args)
  | "caml_neon_int64x2_bitwise_xor" -> Some (Eorq_s64, args)
  | "caml_neon_int64x2_neg" -> Some (Negq_s64, args)
  | "caml_neon_int32x4_slli" ->
    let n, args = extract_constant args ~max:32 op in
    Some (Shlq_n_u32 n, args)
  | "caml_neon_int64x2_slli" ->
    let n, args = extract_constant args ~max:64 op in
    Some (Shlq_n_u64 n, args)
  | "caml_neon_int32x4_ushl" -> Some (Shlq_u32, args)
  | "caml_neon_int64x2_ushl" -> Some (Shlq_u64, args)
  | "caml_neon_int32x4_srli" ->
    let n, args = extract_constant args ~max:32 op in
    Some (Shrq_n_u32 n, args)
  | "caml_neon_int64x2_srli" ->
    let n, args = extract_constant args ~max:64 op in
    Some (Shrq_n_u64 n, args)
  | "caml_neon_int32x4_sshl" -> Some (Shlq_s32, args)
  | "caml_neon_int64x2_sshl" -> Some (Shlq_s64, args)
  | "caml_neon_int32x4_srai" ->
    let n, args = extract_constant args ~max:32 op in
    Some (Shrq_n_s32 n, args)
  | "caml_neon_int64x2_srai" ->
    let n, args = extract_constant args ~max:64 op in
    Some (Shrq_n_s64 n, args)
  | "caml_neon_int32x4_extract" ->
    let lane, args = extract_constant args ~max:3 op in
    Some (Getq_lane_s32 { lane }, args)
  | "caml_neon_int64x2_extract" ->
    let lane, args = extract_constant args ~max:1 op in
    Some (Getq_lane_s64 { lane }, args)
  | "caml_neon_int32x4_insert" ->
    let lane, args = extract_constant args ~max:3 op in
    Some (Setq_lane_s32 { lane }, args)
  | "caml_neon_int64x2_insert" ->
    let lane, args = extract_constant args ~max:1 op in
    Some (Setq_lane_s64 { lane }, args)
  | "caml_neon_int32x4_dup" -> Some (Dupq_lane_s32 { lane = 0 }, args)
  | "caml_neon_int32x4_dup_lane" ->
    let lane, args = extract_constant args ~max:3 op in
    Some (Dupq_lane_s32 { lane }, args)
  | "caml_neon_int64x2_dup" -> Some (Dupq_lane_s64 { lane = 0 }, args)
  | "caml_neon_int64x2_dup_lane" ->
    let lane, args = extract_constant args ~max:1 op in
    Some (Dupq_lane_s64 { lane }, args)
  | _ -> None

let select_operation_cfg op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Operation.Specific (Isimd op), args)

let pseudoregs_for_operation (simd_op : Simd.operation) arg res =
  match Simd_proc.register_behavior simd_op with
  | Rs32x4_Rs32_to_First _ | Rs64x2_Rs64_to_First _ ->
    let arg = Array.copy arg in
    let res = Array.copy res in
    assert (not (Reg.is_preassigned arg.(0)));
    arg.(0) <- res.(0);
    arg, res
  | Rf32x2_Rf32x2_to_Rf32x2 | Rf32x4_Rf32x4_to_Rf32x4 | Rf64x2_Rf64x2_to_Rf64x2
  | Rs64x2_Rs64x2_to_Rs64x2 | Rf32x4_Rf32x4_to_Rs32x4 | Rs32x4_to_Rs32x4
  | Rs32x4_to_Rf32x4 | Rf32x4_to_Rf32x4 | Rf32x4_to_Rs32x4 | Rf32x2_to_Rf64x2
  | Rf64x2_to_f32x2 | Rs32x2_to_Rf64x2 | Rf64x2_to_Rs32x2 | Ri8x16_to_Ri8x16
  | Ri8x16_Ri8x16_to_Ri8x16 | Rs64x2_to_Rs64x2 | Rf64x2_to_Rs64x2
  | Rf64x2_Rf64x2_to_Rs64x2 | Rs32x4_Rs32x4_to_Rs32x4 | Rf32_Rf32_to_Rf32
  | Rf64_Rf64_to_Rf64 | Rf32_to_Rf32 | Rf64_to_Rf64 | Rf32_to_Rs64
  | Rs64x2_to_Rs64 _ | Rs32x4_to_Rs32 _ | Rs32x4lane_to_Rs32x4 _
  | Rs64x2lane_to_Rs64x2 _ | Rf64x2_to_Rf64x2 ->
    arg, res

(* See `amd64/simd_selection.ml`. *)

let vector_width_in_bits = 128

let vectorize_operation _ ~arg_count:_ ~res_count:_ ~alignment_in_bytes:_
    (_ : Operation.t list) :
    Vectorize_utils.Vectorized_instruction.t list option =
  None

(* Error report *)

let report_error ppf = function
  | Bad_immediate msg -> Format.pp_print_string ppf msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
