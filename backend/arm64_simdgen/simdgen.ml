(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Parse [arm64/advsimd.csv] and output [Arm64_simd_defs.instr] definitions.

   [advsimd.csv] was retrieved from
   https://github.com/ARM-software/acle/blob/main/tools/intrinsic_db/advsimd.csv
   commit 91096242b549f4590ba534ae0d9411d613020317. *)

open Amd64_simd_defs
open Printf

exception Unsupported

let all_mnemonics =  Hashtbl.create 1024

let all_instructions = Hashtbl.create 8120

let first_word str =
  match String.split_on_char ' ' str with
  | [fst] -> fst, ""
  | fst :: rest -> fst, String.concat " " rest
  | _ -> failwith str

let support_on_a64 arches =
  arches |> String.split_on_char '/' |>
  List.exists (String.equal "A64")

let parse_args args decl =
  List.iter2 (fun param arg ->
      match param.typ with
      |
    )
    decl.params args

let parse_vector_reg_name reg_name lane =

let reg_name = parse_vector_reg_name reg_name in
{ arg_name; reg_name; lane = parse_interval lane }

let parse_vector_reg_name n =
  let n =
    match n with
    | "8B" -> V8B
    | "16B" -> V16B
    | "4H" -> V4H
    | "8H" -> V8H
    | "2S" -> V2S
    | "4S" -> V4S
    | "1D" -> V1D
    | "2D" -> V2D
    | _ -> raise Unsupported
  in
  (Vector n)

let parse_scalar_reg_name n =
  let n =
    match n with
    | "B" -> B
    | "H" -> H
    | "S" -> S
    | "D" -> D
    | "Q" -> Q
    | _ -> raise Unsupported
  in
  (Scalar n)

let parse_vector_lane_name n lane =
  let reg_name =
    if String.length n = 1 then
      parse_scalar_reg_name n
    else
      parse_vector_reg_name n
  in
  match int_of_string_opt lane with
  | Some n -> Neon (Lane (reg_name * n)), None
  | None -> Neon reg_name, Some lane

let parse_reg n =
  let reg_name =
    match first_letter n with
    | "W" -> GP W
    | "X" -> GP X
    | n -> Neon (parse_scalar_reg_name n)
  in
  { arg_name = n; reg_name; lane = None; }

let parse_vector_loc operand =
  match String.split_on_char '.' with
  | [arg_name;reg_name] ->
      (
        match Misc.String.split_on_chars ['[';']'] with
        | [reg_name] ->
            let reg_name = Neon (parse_vector_reg_name reg_name) in
            { arg_name; reg_name; lane_name = None }
        | [reg_name;lane] ->
            let reg_name, lane_name = parse_vector_lane_name reg_name lane in
            { arg_name; reg_name; lane_name; }
        | _ -> raise Unsupported)
  | _ -> raise Unsupported

let parse_operands operands =
  List.fold_left (fun acc operand ->
      match first_char operand with
      | "#" ->
          (let n = chop_first operand in
           match int_of_string_opt n with
           | Some n -> Imm n
           | None -> Interval n)
      | "V" -> Reg (parse_vector_loc operand)
      | _ -> Reg (parse_reg operand)
    []
    operands
  |> List.rev |> arr

let parse_result res decl =
  match String.split_on_char ' ' res with
  | [] ->
      assert (Option.is_none decl.ret);
      None
  | [loc;"->"; "result"] ->
      assert (Option.is_some decl.ret);
      parse_loc loc args
  | _ -> raise Unsupported

let parse_line line =
  try
    match String.split_on_char '\t' line with
    | [decl;args;instr;res;arches] ->
        assert (supported_on_a64 arches);
        let decl = parse_decl decl in
        let arg_locs = String.split_on_char ';' args |> parse_args decl in
        let res_loc = parse_result res decl in
        let res = if equal_loc args in
        let mnemonic, operands = first_word instr in
        let operands = String.split_on_char ',' operands |> parse_operands param_to_arg in

        let instr = parse_instr mnemonic operands args res in
    | _ -> None
  with
  | Unsupported -> None

let register_mnemonic n =
  (match Hashtbl.find_opt all_mnemonics n with
  | Some n -> ()
  | None -> Hashtbl.add all_mnemonics n ());

let register def =
  Hashtbl.add all_intrinsics def ()

(* CR gyorsh: parse on the fly, output to separate files of menmonics,
   decls and intrinsics. *)
let parse in_channel =
  let rec parse_def acc =
    match In_channel.input_line in_channel with
    | None -> acc
    | Some line ->
        let acc =
          match  parse_line line with
          | None -> acc
          | Some def ->
              register def;
              def::acc
        in
        parse_def acc
  in
  parse_def []

let parse_and_gen spec =
  let instrs = In_channel.with_open_text spec parse in
  print_endline "(* Generated by backend/arm64_simdgen/simdgen.ml *)";
  print_newline;
  print_endline "open Arm64_simd_defs";
  print_newline;
  print_endline "type mnemonic = ";
  Hashtbl.iter (fun n () -> Printf.printf "| %s\n" n) all_mnemonics;
  print_newline;
  Hashtbl.iter (fun n () -> Printf.printf "| %s\n" n) all_intrinsics;

let () = match Sys.argv with [spec] -> parse_and_gen spec | _ -> assert false
