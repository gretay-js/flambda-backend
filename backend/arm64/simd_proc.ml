(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

(* SIMD register behavior for ARM64 *)

(*
 *  [Rf32] Float32 in register (not stack)
 *  [Ri64] Int in register (not stack)
 *)
type register_behavior =
  | Rf32_Rf32_to_RegF32
  | Rf32_to_Rf32
  | Rf32_to_Ri64

let register_behavior (op : Simd.operation) =
  match op with
  (* unary *)
  | Round_f32_i64 -> Rf32_to_Ri64
  | Round_f32 _ -> Rf32_to_Rf32
  (* binary *)
  | Fmin_f32 | Fmax_f32 | Zip1_f32 | Min_scalar_f32 | Max_scalar_f32 ->
    Rf32_Rf32_to_RegF32
