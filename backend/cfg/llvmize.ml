(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Greta Yorsh, Jane Street                          *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list

type error = Asm_generation of (string * int)

exception Error of error

module Ident : sig
  (** Unnamed LLVM identifiers that start with "%".  This includes
      basic blocks, unnamed function parameters, and temporaries
      (virtual registers).  *)
  type t

  val print : Format.formatter -> t -> unit

  module Gen : sig
    (** per-function counter for generating identifiers *)
    type ident = t

    type t

    val create : unit -> t

    val get_fresh : t -> ident
  end
end = struct
  (** block labels and  *)
  type t = int

  let print fmt t = Format.fprintf fmt "%d" t

  module Gen = struct
    type ident = t

    type nonrec t = { mutable next : ident }

    let create () = { next = Label.to_int (Cmm.new_label ()) }

    let get_fresh t =
      let res = t.next in
      t.next <- succ res;
      res
  end
end

type fun_info =
  { ident_gen : Ident.Gen.t;
    reg2ident : Ident.t Reg.Tbl.t;  (** Map register's stamp to identifier  *)
    label2ident : Ident.t Label.Tbl.t
        (** Map label to identifier. Avoid clashes between pre-existing Cfg labels and unnamed identifiers. *)
  }

type t =
  { llvmir_filename : string;
    oc : Out_channel.t;
    ppf : Format.formatter;
    asm_filename : string; (* fields below should be reset for every function *)
    current_fun_info : fun_info ref
  }

let create llvmir_filename asm_filename =
  let oc = Out_channel.open_text llvmir_filename in
  let ppf = Format.formatter_of_out_channel oc in
  { llvmir_filename; asm_filename; oc; ppf; current_fun_info = None }

let get_ident_for_label t label =
  match Label.Tbl.find_opt t.label2ident label with
  | Some ident -> ident
  | None ->
    let ident = Ident.Gen.get_fresh t.ident_gen in
    Label.Tbl.add t.label2ident label ident;
    ident

module F = struct
  open Format

  let pp_indent t = fprintf t.ppf "  "

  let pp_comma ppf () = fprintf ppf ", "

  let line t = kfprintf pp_print_newline t.ppf

  let pp_dbg ppf dbg =
    (* CR gyorsh: emit metadata debuginfo. For now just emit a comment, to help
       debug llvmize pass. Emit a block comment in the case there is a newline
       after it. *)
    fprintf ppf " /* !dbg <id> %a */ " Debuginfo.print_compact dbg

  let pp_dbg_newline dbg ppf = fprintf ppf ", %a\n" dbg

  let ins t dbg =
    pp_indent t;
    kfprintf (pp_dbg_newline dbg) t.ppf

  let source_filename t s = line t "source_filename = %s" s ()

  let machtyp_component (m : Cmm.machtype_component) =
    match m with
    | Val -> "ptr" (* into ocaml heap *)
    | Addr -> "ptr" (* *)
    | Int -> "i63"
    | Float -> "f64"
    | Vec128 -> "f128"
    | Float32 -> "f32"
    | Valx2 -> assert false (* CR gyorsh: "ptr" is wrong size *)

  let pp_machtyp_component ppf c = Format.fprintf ppf "%s" (machtyp_component c)

  let pp_machtyp ppf machtyp =
    machtyp |> Array.to_seq
    |> pp_print_seq ~pp_sep:pp_comma pp_machtyp_component ppf

  let pp_label_ident t ppf label =
    let ident = get_ident_for_label t label in
    fprintf ppf "%%%a" Label.print label

  let pp_label t ppf label = fprintf ppf "label %a" (pp_label_ident t) label

  let pp_label_def t ppf label =
    let ident = get_ident_for_label t label in
    fprintf ppf "%a:" Ident.print ident

  let block_label_with_predecessors t label preds =
    fprintf t.ppf "%a:" (pp_label_def t) label;
    (* CR gyorsh: map label to unique ident *)
    if not (Misc.Stdlib.List.is_empty preds)
    then
      fprintf t.ppf
        "                                                ; preds = %a\n"
        (pp_print_list ~pp_sep:pp_comma pp_label_ident)
        preds

  let define t fun_name fun_args fun_res_type fun_dbg fun_attrs pp_body =
    line t "define %a @%s(%a) %a %a {" pp_machtyp fun_res_type fun_name
      pp_machtyp fun_args (pp_attrs t) fun_attrs (pp_dbg t) fun_dbg pp_body ();
    pp_body t;
    line "}"

  let alloca t (reg : Reg.t) =
    (* %3 = alloca i32, align 4 *)
    line t

  let terminator t (i : Cfg.terminator Cfg.instruction) =
    match i.desc with
    | Never -> assert false
    | Always lbl -> ins t i.dbg "br %a" pp_label_ident lbl
    | Parity_test b ->
      (* Check if the argument is even *)
      let arg = i.arg.(0) in
      let is_odd = fresh_ident t in
      ins t i.dbg "%a = trunc %a to i1" pp_ident is_odd (pp_typ_and_ident t) arg;
      (* reverse the branches *)
      ins t i.dbg "br i1 %a, %a, %a" pp_ident is_odd pp_label b.ifnot pp_label
        b.ifso
    | Truth_test b ->
      (* Check if the argument is true. *)
      let arg = i.arg.(0) in
      let is_true = fresh_ident t in
      ins t i.dbg "%a = trunc %a to i1" pp_ident is_true (pp_typ_and_ident t)
        arg;
      ins t i.dbg "br i1 %a, %a, %a" pp_ident is_odd pp_label b.ifso pp_label
        b.ifnot
    | Float_test float_test | Int_test int_test | Switch labels ->
      Misc.fatal_error "Not implemented"
    | Return ->
      let arg = i.arg.(0) in
      ins t i.dbg "ret %a" (pp_typ_and_ident t) arg
    | Raise kind | Tailcall_self { destination } ->
      Misc.fatal_error "Not implemented"
    | Tailcall_func func_call_operation -> Misc.fatal_error "Not implemented"
    | Call_no_return { op = _; label_after = _ } ->
      Misc.fatal_error "Not implemented"
    | Call { op = _; label_after = _ } -> Misc.fatal_error "Not implemented"
    | Prim { op = _; label_after = _ } -> Misc.fatal_error "Not implemented"
end

let current_compilation_unit = ref None

let llvmir_to_assembly t =
  (* CR-someday gyorsh: add other optimization flags and control which passes to
     perform. *)
  let cmd = Config.asm :: Misc.debug_prefix_map_flags () in
  Ccomp.command
    (String.concat " "
       (cmd
       @ [ "-o";
           Filename.quote t.asm_filename;
           "-O3";
           "-S";
           "-x ir";
           Filename.quote t.llvmir_filename ]))

let close_out () =
  match !current_compilation_unit with
  | None -> ()
  | Some t ->
    (* Exception raised during llvmize, keep .ll file. *)
    Out_channel.close t.oc;
    current_compilation_unit := None

let open_out ~asm_filename ~output_prefix =
  (* Create LLVM IR file for the current compilation unit. *)
  let llvmir_filename = output_prefix ^ ".ll" in
  current_compilation_unit := Some (create llvmir_filename asm_filename)

let fun_attrs _t _codegen_options : string list =
  (* CR gyorsh: translate and communicate to llvm backend *)
  []

let alloca_regs t = Reg.Set.iter (F.alloca t) t.regs

let cfg (cl : CL.t) =
  let t = Option.get !current_compilation_unit in
  let layout = CL.layout cl in
  let cfg = CL.cfg cl in
  (* CR gyorsh: handle unboxed return type (where is this info? do we need to
     find [Return] instruction to find it? if so, add a field to [Cfg.t] to
     store it explicitly. *)
  let { blocks;
        fun_name;
        fun_args;
        fun_codegen_options;
        fun_dbg;
        entry_label;
        fun_contains_calls = _ (* not used at this point *);
        fun_num_stack_slots = _ (* only available after regalloc *);
        fun_poll = _ (* not needed after poll insertion *);
        fun_ret_type
      } =
    cfg
  in
  let pp_block label =
    let block = Label.Tbl.find blocks label in
    let preds = Cfg.predecessor_labels block in
    if Label.equal cfg.entry_label label
    then
      if not (List.is_empty preds)
      then
        Misc.fatal_errorf "Entry label must not have predecessors"
          F.block_label_with_predecessors t label preds;
    DLL.iter ~f:(F.basic t) block.body;
    F.terminator t block.terminator
  in
  let pp_body () =
    collect_regs t cfg;
    alloca_regs t;
    DLL.iter ~f:pp_block layout
  in
  let fun_attrs = fun_attrs t fun_codegen_options in
  F.define t cfg.fun_name cfg.fun_args cfg.fun_res_type cfg.fun_dbg fun_attrs
    pp_body

let data _d = Misc.fatal_error "Not implemented"

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let begin_assembly () = ()

let end_assembly ~sourcefile =
  let t = Option.get !current_compilation_unit in
  (* Emit source_file *)
  Option.iter (F.source_filename t) sourcefile;
  (* Close channel to .ll file *)
  Out_channel.close t.oc;
  (* Call clang to compile .ll to .s *)
  let ret_code = llvmir_to_assembly t in
  if ret_code <> 0 then raise (Error (Asm_generation (sourcefile, ret_code)));
  if not !Flambda_backend_flags.dump_llvmir then remove_file t.llvmir_filename;
  current_compilation_unit := None

(* CR-someday gyorsh: currently, llvm backend can be selected at the compilation
   unit granularity only. It could be controlled at the function granularity. *)
(* CR-someday gyorsh: Compiling directly to .o would involve more changes to
   [Asmgen], [Asmlink], and drivers. It would improve compilation speed but not
   as much as avoiding the textual representation entirely by linking in the
   llvm library statically. *)
(* CR-someday gyorsh: we could set [binary_backend_available] but it is
   currently too tightly coupled with the [internal_assembler], especially in
   [asmlink] for shared libraries. *)
(* CR gyorsh: how to emit data_begin/end and code_begin/end symbol? Do we still
   need both of them with ocaml ? *)
(* CR gyorsh: assume 64-bit architecture *)
(* Error report *)

let report_error ppf = function
  | Asm_generation (fn, ret_code) ->
    Format.fprintf ppf "Error producing assembly code for %s: %d" fn ret_code

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
