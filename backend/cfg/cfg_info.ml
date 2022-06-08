(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

(* CR gyorsh: same as in ocamlfdo modules Block_info + Cfg_info +
   Linearid_profile, adapted to compile with Stdlib, removed some debug printing
   that used sexp. *)
let verbose = ref true

module CL = Cfg_with_layout
module F = Fdo_profile
module Loc = F.Loc
module Execount = F.Execount
module Func = F.Func
module String = Misc.Stdlib.String
module Int = Numbers.Int
module Raw_addr = F.Raw_addr
module Raw_addr_pair = F.Raw_addr_pair

module Block_info : sig
  (* CR-someday gyorsh: Improve different option fields. The reason for them is
     that we don't always have all of the information: Loc.t, label, linearid.
     We can have linearid from the cfg without having Loc.t if the the address
     didn't in perf profile. We can reconstruct it but its expensive and we
     currently only use it for debugging. Maybe use variant to describe the kind
     of location we have? *)

  (* Successor info *)
  type b =
    { target : Loc.t option;
      target_label : Label.t option;
      target_id : int option;
      (* is the target intraprocedural? *)
      intra : bool;
      fallthrough : bool;
      (* true for fallthrough targets where counts are inferred from LBR; false
         for branches that appeared explicitly in LBR *)
      mutable taken : Execount.t;
      mutable mispredicts : Execount.t
    }

  (* call site info *)
  type c =
    { callsite : Loc.t;
      mutable callees : b list
    }

  (* Execution counts for a basic block *)
  type t = private
    { label : Label.t;
      (* Instruction id for the first and last instruction. *)
      (* [first_id] can be the same as terminator_id if body is empty *)
      first_id : int;
      terminator_id : int;
      (* Number of times this block was executed. *)
      mutable count : Execount.t;
      (* Info about branch targets *)
      mutable branches : b list;
      (* Info about call targets *)
      mutable calls : c list
    }

  val mk : label:Label.t -> first_id:int -> terminator_id:int -> t

  (* in-place update of mutable fields *)
  val add : t -> count:Execount.t -> unit

  (* Maintain unique targets *)
  val _add_call : t -> callsite:Loc.t -> callee:b -> unit

  val add_branch : t -> b -> unit
end = struct
  (* CR-someday gyorsh: Improve different option fields. The reason for them is
     that we don't always have all of the information: Loc.t, label, linearid.
     We can have linearid from the cfg without having Loc.t if the the address
     didn't in perf profile. We can reconstruct it but its expensive and we
     currently only use it for debugging, see bolt_profile. Maybe use variant to
     describe the kind of location we have? *)

  (* Successor info *)
  type b =
    { target : Loc.t option;
      target_label : Label.t option;
      target_id : int option;
      (* is the target intraprocedural? *)
      intra : bool;
      fallthrough : bool;
      (* true for fallthrough targets where counts are inferred from LBR; false
         for branches that appeared explicitly in LBR *)
      mutable taken : F.Execount.t;
      mutable mispredicts : Execount.t
    }

  (* Function Must have at least one of target or target_label *)
  (* fallthrough blocks that were inferred from LBR but not directly sampled
     don't have a corresponding raw address. We don't define their target
     location. *)

  (* call site info *)
  type c =
    { callsite : Loc.t;
      mutable callees : b list
    }

  (* Execution counts for a basic block *)
  type t =
    { (* Label of this block *)
      label : Label.t;
      (* Instruction id for the first and last instruction. *)
      (* [first_id] can be the same as terminator_id if body is empty *)
      first_id : int;
      terminator_id : int;
      mutable count : Execount.t;
      (* Number of times this block was executed. *)
      mutable branches : b list;
      (* Info about branch targets *)
      mutable calls : c list (* Info about call targets *)
    }

  let mk ~label ~first_id ~terminator_id =
    assert (not (terminator_id = 0));
    assert (not (first_id = 0));
    assert (not (label = 0));
    { label; count = 0L; branches = []; calls = []; first_id; terminator_id }

  let add t ~count = t.count <- Execount.add t.count count

  let find branches branch =
    (* List.partition_tf *)
    ListLabels.find_opt branches ~f:(fun b ->
        match b.target, b.target_label, b.target_id with
        | Some t, _, _ when Some t = branch.target ->
          assert (
            b.target_label = branch.target_label
            && b.target_id = branch.target_id);
          true
        | None, Some tl, _ when Some tl = branch.target_label ->
          assert (Option.is_none branch.target && b.target_id = branch.target_id);
          true
        | None, None, Some tid when Some tid = branch.target_id ->
          assert (
            Option.is_none branch.target && Option.is_none branch.target_label);
          true
        | None, None, None -> assert false
        | _ -> false)

  let _add_call t ~callsite ~callee =
    (* Find the callsite's info *)
    match ListLabels.find_opt t.calls ~f:(fun c -> c.callsite = callsite) with
    | None ->
      let c = { callsite; callees = [callee] } in
      t.calls <- c :: t.calls
    | Some c -> (
      match
        (* Invariant: unique entry per call target. *)
        (* Find call target entry and update it. *)
        find c.callees callee
      with
      | None -> c.callees <- callee :: c.callees
      | _ -> assert false)

  (* Merge maintain unique targets *)
  let add_branch t b =
    match find t.branches b with
    | None -> t.branches <- b :: t.branches
    | Some br ->
      (* It must have been one Linear instruction emitted as multiple branch
         instruction to the same target. This should never happen, if the cfg is
         simplified, but now it can happen from Lcondbranch3, which may have the
         same target in 2 of its branches. *)
      assert (Bool.equal b.intra br.intra);
      assert (Bool.equal b.fallthrough br.fallthrough);
      if !verbose
         && not
              (Execount.equal b.mispredicts 0L
              && Execount.equal br.mispredicts 0L)
      then Printf.printf "Non-zero mispredict value for the same target\n";
      br.mispredicts <- Execount.add br.mispredicts b.mispredicts;
      br.taken <- Execount.add br.taken b.taken
end

module BB = struct
  let body t = t.Cfg.body

  let terminator t = t.Cfg.terminator

  let start t = t.Cfg.start
end

(** Map basic blocks of this function to breakdown of execution counts *)
type blocks = Block_info.t Label.Tbl.t

type t =
  { cl : CL.t;
    func : Func.t;
    mutable malformed_traces : Execount.t;
        (** number of fallthrough traces that didn't correspond to the cfg *)
    blocks : blocks;
    id_to_label : Label.t Int.Tbl.t
        (** Map id of instruction to label of the block that contains the
            instruction. Used for mapping perf data back to linear IR. *)
  }

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_counter t label =
  match get_block t label with None -> None | Some bi -> Some bi.count

let successor_labels b = Cfg.successor_labels ~normal:true ~exn:false b

let get_block_successor_counter t label ~successor =
  match get_block t label with
  | None -> None
  | Some bi -> (
    let b : Block_info.b option =
      ListLabels.find_opt bi.branches ~f:(fun (b : Block_info.b) ->
          match b.target_label with None -> false | Some l -> l = successor)
    in
    match b with None -> None | Some b -> Some b.taken)

let malformed_traces t = t.malformed_traces

let compute_id_to_label t =
  let add_exn ~key ~data =
    if Int.Tbl.mem t.id_to_label key
    then Misc.fatal_errorf "Cfg_info: Linearid %d already present" key;
    Int.Tbl.add t.id_to_label key data
  in
  let f label block =
    ListLabels.iter
      ~f:(fun (i : _ Cfg.instruction) -> add_exn ~key:i.id ~data:label)
      (BB.body block);
    let terminator = BB.terminator block in
    add_exn ~key:terminator.id ~data:label
  in
  Cfg.iter_blocks (CL.cfg t.cl) ~f

let init cl func =
  let t =
    { cl;
      func;
      malformed_traces = 0L;
      blocks = Label.Tbl.create 17;
      id_to_label = Int.Tbl.create 31
    }
  in
  compute_id_to_label t;
  t

let entry_label t =
  let cfg = CL.cfg t.cl in
  Cfg.entry_label cfg

let id_to_label t id =
  match Int.Tbl.find_opt t.id_to_label id with
  | None ->
    Misc.fatal_errorf
      "Cannot find CFG label for Linear ID %d in func %dHint: Cannot apply the \
       profile to the code. If the code may have changed since the profile was \
       created, try to get a fresh profile. "
      id t.func.id
  | Some lbl ->
    if !verbose then Printf.printf "Found label %d for id %d in map\n" lbl id;
    Some lbl

let terminator_to_string cfg block =
  let n = successor_labels block |> Label.Set.cardinal in
  match (BB.terminator block).desc with
  | Return -> "Return"
  | Raise _ -> "Raise"
  | Tailcall (Self r) -> Printf.sprintf "Tailcall self %d" r.destination
  | Tailcall (Func _) -> "Tailcall"
  | Call_no_return _ -> "Call_no_return"
  | Never ->
    Misc.fatal_errorf "Illegal cfg for %s: block %d terminator is Never"
      (Cfg.fun_name cfg) (BB.start block)
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ ->
    Printf.sprintf "Branch with %d successors" n
  | Switch s ->
    Printf.sprintf "Switch with %d case and %d distinct successors"
      (Array.length s) n

let mal t count =
  if !verbose then Printf.printf "Malformed trace with %Ld counts.\n" count;
  t.malformed_traces <- Execount.add t.malformed_traces count

let first_id block =
  match BB.body block with [] -> (BB.terminator block).id | hd :: _ -> hd.id

(* Some Linear instructions don't generate any code on some platforms. There
   might not be any execounters associated with them.

   Check that [id] can be the first instruction emitted in this block, even
   though the actual first instruction in Linear IR may be different.

   This is used as a safety check when mapping execution counters to blocks.

   We don't know what the backends actually generate, so this check is only an
   approximation. At the time of writing this, these are the only instructions
   that might not result in any code, on some backend.

   Some of these instructions result in dwarf info (such as cfi directives), and
   may even result in a debug .loc line that is not associated with any assembly
   instruction and may bedirectly followed by another .loc.

   Both .loc will be associated with the same code address in Dwarf debug line
   programs, but usually only the last one is recorded in the profile (and
   likely will be, as it is the closed one associated with the address, although
   both .loc will be associated with the same address). *)
let can_be_first_emitted_id id block =
  let open Cfg in
  let rec check_first body =
    match body with
    | [] -> (BB.terminator block).id = id
    | hd :: tl -> (
      if hd.id = id
      then true
      else
        match hd.desc with
        | Reloadretaddr | Prologue | Poptrap -> check_first tl
        | Op Opaque -> check_first tl
        | Op
            ( Move | Spill | Reload | Const_int _ | Const_float _
            | Const_symbol _ | Stackoffset _ | Load _ | Store _ | Intop _
            | Intop_imm _ | Negf | Absf | Addf | Subf | Mulf | Divf | Compf _
            | Floatofint | Intoffloat | Probe _ | Probe_is_enabled _
            | Specific _ | Name_for_debugger _ | Begin_region | End_region )
        | Call _ | Pushtrap _ ->
          false)
  in
  check_first (BB.body block)

let rec last_exn = function
  | [] -> Misc.fatal_errorf "last_exn called with empty list"
  | [last] -> last
  | _ :: tl -> last_exn tl

let get_or_add_block t block =
  let start = BB.start block in
  match Label.Tbl.find_opt t.blocks start with
  | Some b -> b
  | None ->
    let b =
      let terminator = BB.terminator block in
      let terminator_id =
        match terminator.id with
        | 0 ->
          (* use the id of the last instruction in the body *)
          (match terminator.desc with
          | Always _ -> assert true
          | Never | Return | Parity_test _ | Truth_test _ | Float_test _
          | Int_test _ | Switch _ | Raise _ | Call_no_return _ | Tailcall _ ->
            assert false);
          let last = last_exn (BB.body block) in
          last.id
        | n -> n
      in
      let first_id = first_id block in
      let block_start = BB.start block in
      if !verbose
      then
        Printf.printf
          "make new block info for block.start=%d first_id=%d terminator_id=%d\n"
          block_start first_id terminator_id;
      Block_info.mk ~label:block_start ~first_id ~terminator_id
    in
    Label.Tbl.add t.blocks start b;
    b

let record t block ~count =
  let b = get_or_add_block t block in
  Block_info.add b ~count

let get_linearid (loc : Loc.t) = Option.get loc.dbg

(* Find the block in [cfg] that contains [loc] using its linearid *)
let get_block_for_loc t (loc : Loc.t) =
  match loc.dbg with
  | None ->
    (if !verbose
    then
      let rel = Option.get loc.rel in
      Printf.printf "No linearid for loc in func %d at offsets %d\n" rel.id
        rel.offset);
    None
  | Some dbg -> (
    match id_to_label t dbg with
    | None ->
      Misc.fatal_errorf "No cfg label for linearid %d in %d" dbg t.func.id ()
    | Some label -> (
      match Cfg.get_block (CL.cfg t.cl) label with
      | Some block -> Some block
      | None ->
        Misc.fatal_errorf
          "Can't find cfg basic block labeled %d for linearid %d in func %d\n"
          label dbg t.func.id ()))

let record_intra t ~from_loc ~to_loc ~count ~mispredicts =
  let from_block = get_block_for_loc t from_loc in
  let to_block = get_block_for_loc t to_loc in
  match from_block, to_block with
  | None, None ->
    if !verbose
    then Printf.printf "Ignore intra branch count %Ld, can't map to CFG\n" count
  | Some from_block, None ->
    if !verbose
    then
      Printf.printf "Ignore intra branch count %Ld, can't map target to CFG\n"
        count;
    record t from_block ~count
  | None, Some to_block ->
    if !verbose
    then
      Printf.printf "Ignore intra branch count %Ld, can't map source to CFG\n"
        count;
    record t to_block ~count
  | Some from_block, Some to_block ->
    record t from_block ~count;
    record t to_block ~count;
    let from_linearid = get_linearid from_loc in
    let to_linearid = get_linearid to_loc in
    let to_block_first_id = first_id to_block in
    let to_block_start = BB.start to_block in
    let from_block_start = BB.start from_block in
    if !verbose
    then
      Printf.printf
        "Intra branch count %Ld from (id=%d,lbl=%d) to \
         (id=%d,lbl=%d,first_id=%d)\n"
        count from_linearid from_block_start to_linearid to_block_start
        to_block_first_id;
    let bi = get_or_add_block t from_block in
    let b =
      { Block_info.target = Some to_loc;
        target_label = Some to_block_start;
        target_id = Some to_block_first_id;
        intra = true;
        fallthrough = false;
        taken = count;
        mispredicts
      }
    in
    let from_block_terminator = BB.terminator from_block in
    if from_block_terminator.id = from_linearid
    then (
      match
        (* Find the corresponding successor *)
        from_block_terminator.desc
      with
      | Return ->
        (* return from a recursive call *)
        (* target must be right after a call *)
        if !verbose
        then
          Printf.printf "Return from (label=%d) to (label=%d)" from_block_start
            to_block_start
      (* CR-someday gyorsh: We could count the reverse of this edge as a call.
         It's a bit tricky as we need to find the instruction right before the
         to_loc and that is the callsite, and we need to construct callee from
         the entry location of the current function (cfg.entry_label). This will
         count the same call twice if both call and return are sampled. We
         should try to discard matching counts. *)
      (* let call_site = callsite_of_return_site to_loc
       * let cbi = get_or_add_block t (get_block call_site)
       * let callee = { b with
       *                (* start of this function *)
       *                target_label = cfg.entry_label;
       *                target = Some loc?;
       *              } in
       * add_call cbi ~callsite ~callee *)
      | Tailcall (Self r) ->
        if !verbose
        then
          Printf.printf "Tailcall %d from linid=%d from_label=%d %Ld"
            r.destination from_linearid from_block_start count;
        assert (r.destination = to_block_start);
        (* CR-someday gyorsh: count calls *)
        (* Block_info.add_call bi ~callsite:from_loc ~callee:b; *)
        Block_info.add_branch bi b
      | Tailcall (Func Indirect) ->
        (* An indirect tailcall that happens to be a self tailcall is fine.
           Don't call Block_info.add_branch for it because CFG has no
           corresponding successor. *)
        if not (entry_label t = to_block_start) then assert false
      | Call_no_return _ | Tailcall (Func (Direct _)) -> assert false
      | Raise _ ->
        (* target must be a handler block *)
        (* assert (Cfg_builder.is_trap_handler cfg to_block.start) *)
        ()
      | Always _ | Never | Parity_test _ | Truth_test _ | Float_test _
      | Int_test _ | Switch _ ->
        assert (
          to_block_first_id = to_linearid
          || can_be_first_emitted_id to_linearid to_block);
        assert (Label.Set.mem to_block_start (successor_labels from_block));
        Block_info.add_branch bi b)
    else
      ( (* CR-someday gyorsh: record calls *)
        (* recursive call, find the call instruction *) )

let record_exit t (from_loc : Loc.t) (to_loc : Loc.t option) count mispredicts =
  (* Branch going outside of this function. *)
  match get_block_for_loc t from_loc with
  | None ->
    if !verbose
    then
      Printf.printf "Ignore inter branch count %Ld. Can't map to CFG.\n" count
  | Some from_block ->
    record t from_block ~count;
    (* Find the corresponding instruction and update its counters. The
       instruction is either a terminator or a call.*)
    let linearid = get_linearid from_loc in
    let terminator = BB.terminator from_block in
    let bi = get_or_add_block t from_block in
    if terminator.id = linearid
    then
      match (* terminator *)
            terminator.desc with
      | Never ->
        Misc.fatal_errorf "Illegal cfg for block %d: terminator is Never"
          (BB.start from_block)
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Switch _
      | Tailcall (Self _) ->
        (* can't branch outside the current function *)
        if !verbose
        then
          Printf.printf
            "record_exit count=%Ld from_block.start=%d terminator_id=%d.\n"
            count (BB.start from_block) terminator.id;
        assert false
      | Call_no_return _ | Tailcall (Func _) | Return | Raise _ -> (
        match to_loc with
        | None -> ()
        | Some _ ->
          let b =
            { Block_info.target = to_loc;
              target_label = None;
              target_id = None;
              intra = false;
              fallthrough = false;
              taken = count;
              mispredicts
            }
          in
          Block_info.add_branch bi b)
    else
      ( (* Call *)
        (* CR-someday gyorsh: record calls *) )

let record_entry t (to_loc : Loc.t) count _mispredicts =
  (* Branch into this function from another function, which may be unknown. One
     of the following situations:

     Callee: branch target is the first instr in the entry block.

     Return from call: branch target is a label after the call site.

     Exception handler: branch target is a trap handler. *)
  match get_block_for_loc t to_loc with
  | None ->
    if !verbose
    then
      Printf.printf "Ignore inter branch count %Ld. Can't map to CFG.\n" count
  | Some to_block ->
    (* CR-someday gyorsh: find the corresponding instruction and update its
       counters.*)
    record t to_block ~count

(* Depending on the settings of perf record and the corresponding CPU
   configuration, LBR may capture different kinds of branches, including
   function calls and returns. *)
let record_branch t ~(from_loc : Loc.t option) ~(to_loc : Loc.t option)
    ~data:count ~mispredicts =
  (* at least one of the locations is known to be in this function *)
  match from_loc, to_loc with
  | None, None ->
    Misc.fatal_errorf
      "Malformed profile: branch with both source and target locations unknown"
  | None, Some to_loc -> (
    match to_loc.rel with
    | Some rel ->
      if rel.id = t.func.id then record_entry t to_loc count mispredicts
    | _ -> ())
  | Some from_loc, None -> (
    match from_loc.rel with
    | Some rel ->
      if rel.id = t.func.id then record_exit t from_loc to_loc count mispredicts
    | _ -> ())
  | Some from_loc, Some to_loc -> (
    match from_loc.rel, to_loc.rel with
    | Some from_rel, Some to_rel
      when from_rel.id = t.func.id && to_rel.id = t.func.id ->
      record_intra t ~from_loc ~to_loc ~count ~mispredicts
    | Some from_rel, _ when from_rel.id = t.func.id ->
      record_exit t from_loc (Some to_loc) count mispredicts
    | _, Some to_rel when to_rel.id = t.func.id ->
      record_entry t to_loc count mispredicts
    | _ -> assert false)

exception Malformed_fallthrough_trace

module List_helpers = struct
  (** Functions copied from Base.List *)

  (** [drop_while l ~f] drops the longest prefix of [l] for which [f] is
      [true]. *)
  let rec drop_while t ~f =
    match t with hd :: tl when f hd -> drop_while tl ~f | t -> t

  (** [take l n] returns the first [n] elements of [l], or all of [l] if [n >
      length l]. *)
  let take t_orig n =
    if n <= 0
    then []
    else
      let rec loop n t accum =
        if n = 0
        then List.rev accum
        else
          match t with [] -> t_orig | hd :: tl -> loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []

  let findi t ~f =
    let rec loop i t =
      match t with
      | [] -> None
      | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
    in
    loop 0 t
end

let compute_fallthrough_execounts t from_lbl to_lbl count =
  let layout = CL.layout t.cl in
  let cfg = CL.cfg t.cl in
  (* Get the part of the layout starting from from_block up to but not including
     to_block *)
  try
    let fallthrough =
      List_helpers.drop_while ~f:(fun lbl -> not (lbl = from_lbl)) layout
    in
    let fallthrough =
      match List_helpers.findi fallthrough ~f:(fun _ lbl -> lbl = to_lbl) with
      | None -> raise Malformed_fallthrough_trace
      | Some (to_pos, _) -> List_helpers.take fallthrough to_pos
    in
    if !verbose
    then (
      Printf.printf "func %d trace (from_lbl=%d,to_lbl=%d)\n fallthrough: "
        t.func.id from_lbl to_lbl;
      ListLabels.iter fallthrough ~f:(fun lbl -> Printf.printf " %d" lbl);
      Printf.printf "\nlayout=";
      ListLabels.iter layout ~f:(fun lbl -> Printf.printf " %d" lbl);
      Printf.printf "\n");
    (* Check that all terminators fall through *)
    let check_fallthrough src_lbl dst_lbl =
      let block = Option.get (Cfg.get_block cfg src_lbl) in
      let desc = (BB.terminator block).desc in
      match desc with
      | Never ->
        Misc.fatal_errorf "Illegal cfg for block %d: terminator is Never"
          (BB.start block)
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Switch _ ->
        if !verbose
        then (
          Printf.printf
            "check_fallthrough in func %d trace (from_lbl=%d,to_lbl=%d): \
             src_lbl=%d dst_lbl=%d\n\
             src_block.successor_labels:\n"
            t.func.id from_lbl to_lbl src_lbl dst_lbl;
          Label.Set.iter
            (fun lbl -> Printf.printf "%d\n" lbl)
            (successor_labels block));
        if Label.Set.mem dst_lbl (successor_labels block)
        then src_lbl
        else (
          if !verbose
          then
            Printf.printf
              "Malformed fallthrough in func %d trace (from_lbl=%d,to_lbl=%d): \
               src_lbl=%d dst_lbl=%d\n\
               src_block.successor_labels:\n"
              t.func.id from_lbl to_lbl src_lbl dst_lbl;
          raise Malformed_fallthrough_trace)
      | Call_no_return _ | Return | Raise _ | Tailcall _ ->
        if !verbose
        then
          Printf.printf
            "Unexpected terminator %s in fallthrough trace func %d \
             (from_lbl=%d,to_lbl=%d): src_lbl=%d dst_lbl=%d\n\
             src_block.successor_labels:\n"
            (terminator_to_string cfg block)
            t.func.id from_lbl to_lbl src_lbl dst_lbl;
        raise Malformed_fallthrough_trace
    in
    assert (
      from_lbl
      = ListLabels.fold_right fallthrough ~init:to_lbl ~f:check_fallthrough);
    (* Account only for the intermediate blocks in the trace. Endpoint blocks of
       the trace are accounted for when we handled their LBR branches. *)
    let record_fallthrough src_lbl dst_lbl =
      if !verbose
      then
        Printf.printf "record_fallthrough %d->%d count=%Ld\n" src_lbl dst_lbl
          count;
      let dst_block = Option.get (Cfg.get_block cfg dst_lbl) in
      record t dst_block ~count;
      let target_bi = get_or_add_block t dst_block in
      let src_block = Option.get (Cfg.get_block cfg src_lbl) in
      let bi = get_or_add_block t src_block in
      let b =
        { Block_info.target = None;
          target_label = Some dst_lbl;
          target_id = Some target_bi.first_id;
          intra = true;
          fallthrough = true;
          taken = count;
          mispredicts = 0L
        }
      in
      if !verbose then Printf.printf "record_fallthrough: add\n";
      (* Printf.printf  !"record_fallthrough: add b=%{sexp:Block_info.b}\n\
       *   to bi=%{sexp:Block_info.t}\n"
       * b bi; *)
      Block_info.add_branch bi b;
      if !verbose then Printf.printf "record_fallthrough: after add\n";
      (* Printf.printf
       *   !"record_fallthrough: after add:\nbi=%{sexp:Block_info.t}\n"
       *   bi; *)
      src_lbl
    in
    assert (
      from_lbl
      = ListLabels.fold_right fallthrough ~init:to_lbl ~f:record_fallthrough);
    if !verbose
    then
      Printf.printf "recorded healthy trace from %d to %d count %Ld\n" from_lbl
        to_lbl count
    (* printf !"%{sexp:t}\n" t.blocks *)
  with Malformed_fallthrough_trace ->
    (* If the trace is malformed, don't add counts *)
    mal t count

let record_trace t ~(from_loc : Loc.t option) ~(to_loc : Loc.t option)
    ~data:count =
  (* both locations must be in this function *)
  match from_loc, to_loc with
  | None, _ | _, None ->
    if !verbose
    then
      Printf.printf
        "Ignoring trace with count %Ld to or from function is not the same or \
         not known.\n"
        count;
    mal t count
  | Some from_loc, Some to_loc -> (
    match from_loc.rel, to_loc.rel with
    | Some from_rel, Some to_rel
      when from_rel.id = t.func.id && to_rel.id = t.func.id -> (
      match get_block_for_loc t from_loc, get_block_for_loc t to_loc with
      | Some from_block, Some to_block
        when BB.start from_block = BB.start to_block ->
        if !verbose
        then
          Printf.printf
            "No fallthroughs in trace with count %Ld:from_block = to_block\n"
            count;
        record t from_block ~count
      | Some from_block, Some to_block ->
        if !verbose
        then
          Printf.printf
            "trace=> (from_linid=%d,to_linid=%d)=> (from_block=%d,to_block=%d)\n"
            (Option.get from_loc.dbg) (Option.get to_loc.dbg)
            (BB.start from_block) (BB.start to_block);
        compute_fallthrough_execounts t (BB.start from_block)
          (BB.start to_block) count
      | _ ->
        if !verbose
        then
          Printf.printf
            "Ignoring trace with count %Ld:cannot map to_loc or from_loc to \
             cfg blocks.\n"
            count;
        mal t count)
    | _ ->
      if !verbose
      then
        Printf.printf
          "Ignoring trace with count %Ld to or from function is not the same \
           or not known.\n"
          count;
      mal t count)

let record_ip t ~loc ~data:count =
  match loc with
  | None ->
    if !verbose then Printf.printf "Ignore exec count \n, can't find location\n"
  | Some loc -> (
    match get_block_for_loc t loc with
    | None ->
      if !verbose then Printf.printf "Ignore exec count \n, can't map to cfg\n"
    | Some block -> record t block ~count)

(** debug printing functions *)

let dump_branch (b : Block_info.b) =
  match b.target_label with
  | None -> ()
  | Some successor ->
    if b.intra
    then
      Printf.printf "(.L%d%s:%Ld%s)" successor
        (if b.fallthrough then " tr" else "")
        b.taken
        (if b.mispredicts > 0L
        then Printf.sprintf " mis:%Ld" b.mispredicts
        else "")

let dump_call t (c : Block_info.c) =
  let rel =
    match c.callsite.rel with
    | Some rel -> rel
    | None ->
      Misc.fatal_errorf "Malformed cfg_info for func %d: callsite missing"
        t.func.id
  in
  if not (t.func.id = rel.id)
  then
    Misc.fatal_errorf
      "Malformed cfg_info for func %d: callsite mismatch function id %d"
      t.func.id rel.id;
  match c.callsite.dbg with
  | None ->
    Misc.fatal_errorf
      "Malformed cfg_info for func %d: callsite without linear id at offset %d"
      t.func.id rel.offset
  | Some linearid ->
    Printf.printf "\tcallsite %d: " linearid;
    ListLabels.iter c.callees ~f:dump_branch

let dump_block t label =
  Printf.printf ".L%d: " label;
  match get_block t label with
  | None -> Printf.printf "\n"
  | Some b ->
    Printf.printf "%Ld " b.count;
    ListLabels.iter b.branches ~f:dump_branch;
    ListLabels.iter b.calls ~f:(dump_call t);
    Printf.printf "\n"

let dump t =
  Printf.printf "Cfg info for func %d: %Ld\n" t.func.id t.func.count;
  let layout = CL.layout t.cl in
  ListLabels.iter layout ~f:(dump_block t)

let dump_dot t msg =
  let annotate_block label =
    match get_block t label with
    | None -> ""
    | Some bi -> Printf.sprintf "%Ld" bi.count
  in
  let annotate_succ label succ =
    match get_block t label with
    | None -> ""
    | Some bi -> (
      match
        ListLabels.find_opt bi.branches ~f:(fun (b : Block_info.b) ->
            match b.target_label with
            | None -> false
            | Some target -> target = succ)
      with
      | None -> ""
      | Some b ->
        assert b.intra;
        Printf.sprintf "%s%Ld%s"
          (if b.fallthrough then "tr\\r" else "")
          b.taken
          (if b.mispredicts > 0L
          then Printf.sprintf "\\rmis:%Ld" b.mispredicts
          else ""))
  in
  CL.save_as_dot t.cl ~show_instr:false ~annotate_block ~annotate_succ msg

type stats =
  { mutable hit_blocks : int;
    (* num of blocks with a positive execount *)
    mutable hit_blocks_length : int;
    (* number of instructions (basic+terminator) in all blocks with a positive
       execount *)
    mutable length : int (* total number of instructions in the CFG *)
  }

let percent part total =
  if total > 0 then Float.(100. *. (of_int part /. of_int total)) else 0.

let report_stats t =
  let stats = { hit_blocks = 0; hit_blocks_length = 0; length = 0 } in
  let f label block =
    let len = List.length (BB.body block) + 1 (* for terminator *) in
    stats.length <- stats.length + len;
    match get_block t label with
    | None -> ()
    | Some block_info ->
      if block_info.count > 0L
      then (
        stats.hit_blocks <- stats.hit_blocks + 1;
        stats.hit_blocks_length <- stats.hit_blocks_length + len)
  in
  let cfg = CL.cfg t.cl in
  Cfg.iter_blocks ~f cfg;
  let name = Cfg.fun_name cfg in
  Printf.printf
    "** hit_blocks,blocks,hit_blocks_length,length=%d,%d,%d,%d in %s\n"
    stats.hit_blocks
    (List.length (CL.layout t.cl))
    stats.hit_blocks_length stats.length name;
  Printf.printf "** #hit_blocks/#blocks is %.0f%% in %s\n"
    (percent stats.hit_blocks (List.length (CL.layout t.cl)))
    name;
  Printf.printf "** hit_blocks_length/length is %.0f%% in %s\n"
    (percent stats.hit_blocks_length stats.length)
    name

(* Compute detailed execution counts for function [name] using its CFG *)
(* Translate linear ids of this function's locations to cfg labels within this
   function, find the corresponding basic blocks and update their block_info.
   Perform lots of sanity checks to make sure the location of the execounts
   match the instructions in the cfg. *)
let assign_counters addr2loc (func : Func.t) cl =
  let get_loc addr = F.Raw_addr.Tbl.find_opt addr2loc addr in
  let i = init cl func in
  (* Associate instruction counts with basic blocks *)
  Raw_addr.Tbl.iter
    (fun key data ->
      let loc = get_loc key in
      record_ip i ~loc ~data)
    func.agg.instructions;
  (* Associate fall-through trace counts with basic blocks *)
  Raw_addr_pair.Tbl.iter
    (fun key data ->
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      record_trace i ~from_loc ~to_loc ~data)
    func.agg.traces;
  (* report stats *)
  let m = malformed_traces i in
  (if m > 0L
  then
    let total_traces =
      Raw_addr_pair.Tbl.fold
        (fun _ c acc -> Execount.add acc c)
        func.agg.traces 0L
    in

    Printf.printf "Found %Ld malformed traces out of %Ld (%.3f%%)\n" m
      total_traces
      (percent (Execount.to_int m) (Execount.to_int total_traces)));
  (* Associate branch counts with basic blocks *)
  Raw_addr_pair.Tbl.iter
    (fun key data ->
      let mispredicts =
        Option.value
          (Raw_addr_pair.Tbl.find_opt func.agg.mispredicts key)
          ~default:0L
      in
      let from_addr, to_addr = key in
      let from_loc = get_loc from_addr in
      let to_loc = get_loc to_addr in
      if !verbose
      then
        Printf.printf "recording branch: from addr=%Lx  to   addr=%Lx" from_addr
          to_addr;
      (* printf
       *   !"recording branch:\n\
       *     from addr=%{sexp:Raw_addr.t} loc=%{sexp:Loc.t option}\n\
       *     to   addr=%{sexp:Raw_addr.t} loc=%{sexp:Loc.t option}\n"
       *   from_addr from_loc to_addr to_loc; *)
      record_branch i ~from_loc ~to_loc ~data ~mispredicts)
    func.agg.branches;
  if !verbose
  then (
    dump i;
    dump_dot i "annot");
  report_stats i;
  i

(* cfg_info can be saved to a file for later use. It is only useful for
   debugging. It would save recomputing the counters, but it adds another file
   per function or compilation unit. We can't write them to all to one file
   because jenga many processes runs in parallel, all of which might be
   accessing the same file for write. An alternative is a profile service. In
   any case, saving these profiles also adds complexity to the build rules,
   which would decided for every compilation unit based on the existence of cfg
   profile file, whether to read it or to write it. It is probably not worth it
   because recomputing the counters is fairly fast, and not a bottleneck.
   Similarly, we could save the computed layout to file, but it is not worth it
   as it does not take very long to compute it, and it may not be useful if the
   target binary is rebuild with different heuristics. *)

exception Found of t

(** Look for profile for function using its [name]. If not found, look for
    profiles for [alternatives]. Apply the profile to the cfg to compute
    detailed execution counts for blocks and branches. *)
let create (p : F.t) ~fun_name:name cl ~alternatives =
  try
    let f (s : string) =
      match String.Tbl.find_opt p.name2id s with
      | None -> ()
      | Some id ->
        let func = Int.Tbl.find p.functions id in
        if func.count > 0L && func.has_linearids
        then (
          Printf.printf
            "Found profile for function %s with %Ld samples %s (func id = %d)"
            name func.count
            (if String.equal name s then "" else "using near match " ^ s)
            id;
          if !verbose
          then (
            Printf.printf "compute_cfg_execounts for %s\n" name;
            CL.save_as_dot cl "execount");
          let t =
            Profile.record_call ~accumulate:true "cfg_info" (fun () ->
                assign_counters p.addr2loc func cl)
          in
          raise (Found t))
    in
    ListLabels.iter ~f (name :: alternatives);
    if !verbose
    then Printf.printf "Not found profile info for %s with cfg.\n" name;
    None
  with Found cfg_info -> Some cfg_info
