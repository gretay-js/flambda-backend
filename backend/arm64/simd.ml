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

type operation_class = Pure

module Rounding_mode = struct
  type t =
    | Current
    | Neg_inf
    | Pos_inf
    | Zero

  let instruction_suffix = function
    | Neg_inf -> "m"
    | Pos_inf -> "p"
    | Zero -> "z"
    | Current -> "x"

  let print_rounding_mode ppf mode =
    let suffix = instruction_suffix mode in
    Format.pp_print_string ppf suffix

  let equal t1 t2 =
    match t1, t2 with
    | Current, Current | Neg_inf, Neg_inf | Pos_inf, Pos_inf | Zero, Zero ->
      true
    | (Current | Neg_inf | Pos_inf | Zero), _ -> false
end

type operation =
  | Round_f32 of Rounding_mode.t
  | Round_f32_i64
  | Zip1_f32
  (* [Min_scalar_f32/Max_scalar_f32] are emitted as a sequence of instructions
     that matches amd64 semantics of the same intrinsic
     [caml_simd_float32_min/max], regardless of the value of [FPCR.AH]. *)
  | Min_scalar_f32
  | Max_scalar_f32
  (* [Fmin/Fmax] are emitted as the corresponding arm64 single instructions. *)
  | Fmin_f32
  | Fmax_f32

let instr_size op =
  match op with
  | Round_f32 _ | Round_f32_i64 | Min_scalar_f32 | Max_scalar_f32 | Zip1_f32
  | Fmin_f32 | Fmax_f32 ->
    1

let emit_opcode op =
  match op with
  | Round_f32 rounding_mode ->
    "frint" ^ Rounding_mode.instruction_suffix rounding_mode
  | Round_f32_i64 -> "fcvtns"
  | Fmin_f32 -> "fmin"
  | Fmax_f32 -> "fmax"
  | Zip1_f32 -> "zip1"
  | Min_scalar_f32 -> "Min_scalar_f32: instruction sequence"
  | Max_scalar_f32 -> "Max_scalar_f32: instruction sequence"

let print_name op =
  match op with
  | Round_f32 _ | Round_f32_i64 | Zip1_f32 | Fmin_f32 | Fmax_f32 ->
    emit_opcode op
  | Min_scalar_f32 -> "min_scalar_f32"
  | Max_scalar_f32 -> "max_scalar_f32"

let print_operation printreg op ppf arg =
  (* CR gyorsh: does not support memory operands (except stack operands). *)
  Format.fprintf ppf "%s %a" (print_name op)
    (Format.pp_print_seq ~pp_sep:Format.pp_print_space printreg)
    (arg |> Array.to_seq)

let equal_operation op1 op2 =
  match op1, op2 with
  | Round_f32 mode, Round_f32 mode' -> Rounding_mode.equal mode mode'
  | Round_f32_i64, Round_f32_i64 -> true
  | Min_scalar_f32, Min_scalar_f32
  | Max_scalar_f32, Max_scalar_f32
  | Fmin_f32, Fmin_f32
  | Fmax_f32, Fmax_f32
  | Zip1_f32, Zip1_f32 ->
    true
  | ( ( Round_f32 _ | Round_f32_i64 | Min_scalar_f32 | Max_scalar_f32 | Fmin_f32
      | Fmax_f32 | Zip1_f32 ),
      _ ) ->
    false

let class_of_operation op =
  match op with
  | Round_f32 _ | Round_f32_i64 | Min_scalar_f32 | Max_scalar_f32 | Fmin_f32
  | Fmax_f32 | Zip1_f32 ->
    Pure

let operation_is_pure op = match class_of_operation op with Pure -> true
