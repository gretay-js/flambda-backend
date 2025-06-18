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

module I = Arm64_ast.Instruction_name
module DSL = Arm64_ast.DSL

type emit_reg =
  | Default (* use machtype Reg.typ *)
  | DSL of (int -> Arm64_ast.Operand.t)

type reg =
  { emit_reg : emit_reg;
    allowed_machtypes : Cmm.machtype
  }

(* CR gyorsh: add mem and other operands? *)
type operand =
  | Reg of reg
  | Imm of int

type res =
  | First_arg (* Result is returned in the first argument operand. *)
  | Res of reg (* Separate operand for result. *)

type 'id t =
  { id : 'id;
    instr : I.t option;
    args : operand array;
    res : res
  }

(* Helpers and statically allocated common expressions *)

let reg_default = Reg Default

let operand_default_float32 =
  Reg { loc = Ddefault; allowed_machtypes = Cmm.typ_float32 }

let operand_default_float =
  Reg { loc = Default; allowed_machtypes = Cmm.typ_float }

let operand_default_int = Reg { loc = Default; allowed_machtypes = Cmm.typ_int }

let operand_vec128 emit_reg =
  Reg { loc = emit_reg; allowed_machtypes = Cmm.typ_vec128 }

let operand_vec128_v2d = operand_vec128 DSL.reg_v2d

let operand_vec128_v4s = operand_vec128 DSL.reg_v4s

let operand_vec128_v2s = operand_vec128 DSL.reg_v2s

let make_binary id instr operand =
  { id; instr = Some instr; args = [| operand; operand |]; res = Res operand }

let make_unary id instr operand =
  { id; instr = Some instr; args = [| operand |]; res = Res operand }
