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

type interval = (int,int)  (* inclusive *)

type reg = { arg_name : string; reg_name : Reg_name.t; lane_name: string option }

type loc =
  | Reg of reg
  | Imm of int
  | Interval of string

type res =
  | First_arg (* Result is returned in the first argument operand. *)
  | Res of loc (* Separate operand for result. *)

module Instr = struct
  (* CR gyorsh: connect to [Instruction.t] instead of using string [mnemonic]  *)
  type t =
    {
      mnemonic : Arm64_ast.Instruction_name.t;
      operands : loc array;
    }
end

module Decl = struct
  (** external declaration of the intrinsic  *)
  type typ =
    | Imm
    | Typ of string

  type param = { typ : typ; name : string }

  type t =
    {
      name : string;
      params : param list;
      ret : typ option;
    }

end

type t =
  {
    decl : Decl.t;
    instr : Arm64_ast.Instruction_name.t;
    arg : loc list;
    res : res;
  }
