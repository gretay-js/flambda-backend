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

open Format

type operation_class = Pure

type rounding_mode =
  | Current
  | Neg_inf
  | Pos_inf
  | Zero

type operation =
  | Round_f32 of rounding_mode
  | Round_f32_i64

let rounding_mode_instruction_suffix = function
  | Neg_inf -> "m"
  | Pos_inf -> "p"
  | Zero -> "z"
  | Current -> "x"

let print_rounding_mode ppf mode =
  let suffix = rounding_mode_instruction_suffix mode in
  Format.pp_print_string ppf suffix

let print_operation printreg op ppf arg =
  match op with
  | Round_f32 mode ->
    fprintf ppf "frint%a %a %a" print_rounding_mode mode printreg arg.(0)
      printreg arg.(1)
  | Round_f32_i64 ->
    fprintf ppf "fcvtzs %a %a" printreg arg.(0) printreg arg.(1)

let equal_operation op1 op2 =
  match op1, op2 with
  | Round_f32 mode, Round_f32 mode' -> mode = mode'
  | Round_f32_i64, Round_f32_i64 -> true
  | Round_f32 _, _ | Round_f32_i64, _ -> false

let class_of_operation op = match op with Round_f32 _ | Round_f32_i64 -> Pure

let operation_is_pure op = match class_of_operation op with Pure -> true
