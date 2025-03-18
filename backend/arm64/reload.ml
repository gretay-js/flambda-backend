(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-42"]
(* Reloading for the ARM 64 bits *)

open Reg

class reload = object (self)

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  match op with
  | Ispecific Imove32 ->
      (* Like Imove: argument or result can be on stack but not both *)
      begin match arg.(0), res.(0) with
      | {loc = Stack s1; _}, {loc = Stack s2; _} when s1 <> s2 ->
          ([| self#makereg arg.(0) |], res)
      | {loc = (Stack _ | Reg _| Unknown); _}, _ ->
          (arg, res)
  end
  | Ispecific(Isimd op) -> Simd_reload.reload_operation self#makereg op arg res
  | Ispecific(Imuladd|Imulsub|Inegmulf|Imuladdf|Inegmuladdf|Imulsubf|Inegmulsubf|Isqrtf|
                Ifar_poll _|Ifar_alloc _|Ishiftarith (_, _)|Ibswap _|Isignext _)
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
     ->
      super#reload_operation op arg res
end

let fundecl f num_stack_slots =
  (new reload)#fundecl f num_stack_slots
