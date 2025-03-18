(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* CSE for ARM64 *)

open Mach
open CSE_utils

let of_simd_class (cl : Simd.operation_class)  =
  match cl with
  | Pure -> Op_pure

class cse = object

inherit CSEgen.cse_generic as super

method! class_of_operation op =
  match op with
  | Ispecific spec ->
    (match spec with
     | Ifar_poll _
     | Ifar_alloc _
     | Ishiftarith _
     | Imuladd
     | Imulsub
     | Inegmulf
     | Imuladdf
     | Inegmuladdf
     | Imulsubf
     | Inegmulsubf
     | Isqrtf
     | Ibswap _
     | Imove32
     | Isignext _ -> Op_pure
     | Isimd op ->
       of_simd_class (Simd.class_of_operation op))
  | Imove | Ispill | Ireload
  | Ifloatop _
  | Icsel _
  | Ireinterpret_cast _ | Istatic_cast _
  | Iconst_int _ | Iconst_float32 _ | Iconst_float _
  | Iconst_symbol _ | Iconst_vec128 _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _ | Iextcall _
  | Istackoffset _ | Iload _ | Istore _ | Ialloc _
  | Iintop _ | Iintop_imm _ | Iintop_atomic _
  | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Ipoll _ | Idls_get
    -> super#class_of_operation op

method! is_cheap_operation op =
  match op with
  | Iconst_int n -> n <= 65535n && n >= 0n
  | Ispecific _
  | Imove | Ispill | Ireload
  | Ifloatop _
  | Icsel _
  | Ireinterpret_cast _ | Istatic_cast _
  | Iconst_float32 _ | Iconst_float _
  | Iconst_symbol _ | Iconst_vec128 _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _ | Iextcall _
  | Istackoffset _ | Iload _ | Istore _ | Ialloc _
  | Iintop _ | Iintop_imm _ | Iintop_atomic _
  | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Ipoll _ | Idls_get
    -> false

end

let fundecl f =
  (new cse)#fundecl f

class cfg_cse = object

  inherit Cfg_cse.cse_generic as super

  method! class_of_operation op =
    match op with
    | Specific spec ->
      (match spec with
       | Ifar_poll _
       | Ifar_alloc _
       | Ishiftarith _
       | Imuladd
       | Imulsub
       | Inegmulf
       | Imuladdf
       | Inegmuladdf
       | Imulsubf
       | Inegmulsubf
       | Isqrtf
       | Ibswap _
       | Imove32
       | Isignext _ -> Op_pure
       | Isimd op ->
         of_simd_class (Simd.class_of_operation op))
    | Move | Spill | Reload
    | Floatop _
    | Csel _
    | Reinterpret_cast _ | Static_cast _
    | Const_int _ | Const_float32 _ | Const_float _
    | Const_symbol _ | Const_vec128 _
    | Stackoffset _ | Load _ | Store _ | Alloc _
    | Intop _ | Intop_imm _ | Intop_atomic _
    | Name_for_debugger _ | Probe_is_enabled _ | Opaque
    | Begin_region | End_region | Poll | Dls_get
      -> super#class_of_operation op

  method! is_cheap_operation op =
    match op with
    | Const_int n -> n <= 65535n && n >= 0n
    | Specific _
    | Move | Spill | Reload
    | Floatop _
    | Csel _
    | Reinterpret_cast _ | Static_cast _
    | Const_float32 _ | Const_float _
    | Const_symbol _ | Const_vec128 _
    | Stackoffset _ | Load _ | Store _ | Alloc _
    | Intop _ | Intop_imm _ | Intop_atomic _
    | Name_for_debugger _ | Probe_is_enabled _ | Opaque
    | Begin_region | End_region | Poll | Dls_get
      -> false

end

let cfg_with_layout cfg_with_layout =
  (new cfg_cse)#cfg_with_layout cfg_with_layout
