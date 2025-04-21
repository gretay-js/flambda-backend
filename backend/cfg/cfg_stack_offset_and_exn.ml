[@@@ocaml.warning "+a-4-9-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Flambda_backend_utils.Doubly_linked_list

(* This module relies on the field `can_raise` of basic blocks but does not rely
   on this field of individual Cfg instructions. This may need to be revisited
   when we remove dead trap handlers and the associated pushtrap/poptrap
   operations. *)
type handler_stack = Label.t list

let compute_stack_offset ~stack_offset ~traps =
  stack_offset + (Proc.trap_size_in_bytes * List.length traps)

let check_and_set_stack_offset :
    'a Cfg.instruction -> stack_offset:int -> traps:handler_stack -> unit =
 fun instr ~stack_offset ~traps ->
  assert (instr.stack_offset = Cfg.invalid_stack_offset);
  Cfg.set_stack_offset instr (compute_stack_offset ~stack_offset ~traps)

let process_terminator :
    stack_offset:int ->
    traps:handler_stack ->
    Cfg.terminator Cfg.instruction ->
    int * handler_stack =
 fun ~stack_offset ~traps term ->
  check_and_set_stack_offset term ~stack_offset ~traps;
  match term.desc with
  | Tailcall_self _
    when stack_offset <> 0 || List.compare_length_with traps 0 <> 0 ->
    Misc.fatal_errorf
      "Cfgize.Stack_offset_and_exn.process_terminator: unexpected handler on \
       self tailcall (id=%a)"
      InstructionId.format term.id
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Call _ | Prim _ ->
    stack_offset, traps

let rec process_basic :
    Cfg.t ->
    stack_offset:int ->
    traps:handler_stack ->
    Cfg.basic Cfg.instruction ->
    int * handler_stack =
 fun cfg ~stack_offset ~traps instr ->
  check_and_set_stack_offset instr ~stack_offset ~traps;
  match instr.desc with
  | Pushtrap { lbl_handler } ->
    update_block cfg lbl_handler ~stack_offset ~traps;
    stack_offset, lbl_handler :: traps
  | Poptrap _ -> (
    match traps with
    | [] ->
      Misc.fatal_errorf
        "Cfgize.Stack_offset_and_exn.process_basic: trying to pop from an \
         empty stack (id=%a)"
        InstructionId.format instr.id
    | _ :: traps -> stack_offset, traps)
  | Op (Stackoffset n) -> stack_offset + n, traps
  | Op
      ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
      | Const_symbol _ | Const_vec128 _ | Load _ | Store _ | Intop _
      | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _
      | Reinterpret_cast _ | Probe_is_enabled _ | Opaque | Begin_region
      | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll | Alloc _
        )
  | Reloadretaddr | Prologue ->
    stack_offset, traps
  | Stack_check _ ->
    Misc.fatal_error
      "Cfgize.Stack_offset_and_exn.process_basic: unexpected stack check"

(* The argument [stack_offset] has a different meaning from the field
   [stack_offset] of Cfg's basic_blocks and instructions. The argument
   [stack_offset] refers to the offset derived from Istackoffset operations
   only, whereas the field also includes the trap stack depth in bytes. Both are
   non-negative. See [compute_stack_offset]. *)
and update_block :
    Cfg.t -> Label.t -> stack_offset:int -> traps:handler_stack -> unit =
 fun cfg label ~stack_offset ~traps ->
  let block = Cfg.get_block_exn cfg label in
  let was_invalid =
    if block.stack_offset = Cfg.invalid_stack_offset
    then true
    else (
      assert (block.stack_offset = compute_stack_offset ~stack_offset ~traps);
      false)
  in
  if was_invalid
  then (
    block.stack_offset <- compute_stack_offset ~stack_offset ~traps;
    let stack_offset, traps =
      DLL.fold_left block.body ~init:(stack_offset, traps)
        ~f:(fun (stack_offset, traps) instr ->
          process_basic cfg ~stack_offset ~traps instr)
    in
    let stack_offset, traps =
      process_terminator ~stack_offset ~traps block.terminator
    in
    (* non-exceptional successors *)
    Label.Set.iter
      (update_block cfg ~stack_offset ~traps)
      (Cfg.successor_labels ~normal:true ~exn:false block);
    (* exceptional successor *)
    if block.can_raise
    then (
      assert (Option.is_none block.exn);
      match traps with
      | [] -> ()
      | handler_label :: _ -> block.exn <- Some handler_label))

let compute : Cfg.t -> unit =
 fun cfg -> update_block cfg cfg.entry_label ~stack_offset:0 ~traps:[]
