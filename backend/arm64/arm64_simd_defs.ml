(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-42"]

type emit_reg =
  | Default  (* use machtype Reg.typ *)
  | DSL of (int -> Arm64_ast.Operand.t)

(* CR gyorsh: add mem and other operands or simplify this type *)
type loc =
  | Reg of emit_reg

type operand =
  { loc : loc;
    allowed_machtypes : Cmm.machtype
  }

type res =
  | First_arg (* Result is returned in the first argument operand. *)
  | Res of operand (* Separate operand for result. *)

type 'id t =
  {
    id : 'id
    instr : Arm64_ast.Instruction_name.t option;
    args : operand array;
    res : res;
  }

(* Helpers and statically allocated common expressions *)

let reg_default = Reg Default

let operand_default_float32 = { loc = reg_default; allowed_machtypes = Cmm.typ_float32 }
let operand_default_float = { loc = reg_default; allowed_machtypes = Cmm.typ_float }
let operand_default_int = { loc = reg_default; allowed_machtypes = Cmm.typ_int }
let operand_vec128 emit_reg = { loc = Reg emit_reg; allowed_machtypes = Cmm.typ_vec128 }

let default_binary id instr operand =
  {
    id;
    instr;
    args = [| operand; operand |];
    res = Res operand
  }

let default_unary id instr operand =
  {
    id;
    instr;
    args = [| operand |];
    res = Res operand
  }
