(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2022-2022 Jane Street Group LLC                                  *
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

module String = Misc.Stdlib.String

module Tag = struct
  type t = N | E | D
  let print ppf = function
    | N -> "nor"
    | E -> "exn"
    | D -> "div"
end

module Var : sig
  type t
  val get : string -> Tag.t -> t
  val print : Format.formatter -> t -> unit
end = struct
  type t = { name: string; tag:Tag.t }

  let get name tag =
    { name; tag }

  let print ppf t =
    Format.fprintf ppf "%s.%a@," name Tag.pring tag
end


(** Abstract value for each component of the domain. *)
module V : sig
  type rec t =
    | Top  (** Property may not hold on some paths. *)
    | Safe  (** Property holds on all paths.  *)
    | Bot  (** Not reachable. *)
    | Unresolved of { var:Var.t list; eval:t list -> t }

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val transform : t -> t

  val transform_return : effect:V.t -> t -> t

  val is_not_safe : t -> bool

  val print : Format.formatter -> t -> unit

  val eval : t -> env:(Var.t -> t option) -> t

end = struct
  type rec t =
    | Top
    | Safe
    | Bot
    | Unresolved { var : Var.t list; eval : t list -> t }

  (* Split the first n elements into a one list, the rest into another list. *)
  let split tl n =
    let t1 = List.filteri (fun i _ -> i < n) tl in
    let t2 = List.filteri (fun i _ -> i >= n) tl in
    t1,t2


  let rec join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top, Top -> Top
    | Safe, Bot | Bot, Safe -> Safe
    | Top, Bot | Top, Safe | Bot, Top | Safe, Top -> Top
    | Top, Unresolved _ -> Top
    | Unresolved _, Top -> Top
    | Bot, Unresolved _ -> c2
    | Unresolved _, Bot -> c1
    | Safe, Unresolved { var; eval } | Unresolved {var; eval}, Safe ->
      Unresolved { var;
                   eval = (fun tl -> join (eval tl) Safe);
                 }
    | Unresolved { var = var1; eval = eval1 },
      Unresolved { var = var2; eval = eval2 } ->
      let n = (List.len var1) in
      Unresolved { var = var1@var2;
                   eval = fun tl ->
                     let t1,t2 = split tl n in
                     join (eval1 t1) (eval2 t2)
                 }

  let rec lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top, Top -> true
    | Bot, Safe -> true
    | Bot, Top -> true
    | Safe, Top -> true
    | Top, (Bot | Safe) -> false
    | Safe, Bot -> false
    | Unresolved { var; eval }, v2 ->
      Unresolved { var;
                   eval = fun tl -> lessequal (eval tl) v2;
                 }
    | v1, Unresolved {var; eval} ->
      Unresolved
        { var;
          eval = eval = fun tl -> lessequal v1 (eval tl);
        }
    | Unresolved { var=var1; eval=eval1 },
      Unresolved { var=var2; eval=eval2 } ->
      let n = (List.len var1) in
      Unresolved { var = var1@var2;
                   eval = fun tl ->
                     let t1,t2 = n in
                     lessequal (eval t1) (eval t2)
                 }

  (** abstract transformer (backward analysis) for a statement that violates the property
      but doesn't alter control flow. *)
  let rec transform = function
    | Bot ->
      (* if a return is unreachable from the program location immediately after
         the statement, then return is unreachable from the program location
         immediately before the statement. *)
      Bot
    | Safe | Top -> Top
    | Unresolved { var; eval } ->
      Unresolved { var;
                   eval = fun tl -> transform (eval tl)
                 }

  (* symmetric, reflexive *)
  let rec transform_return t t' =
    match t, t' with
    | Bot, _ - Bot
    | _, Bot -> Bot
    | Top, _ -> Top
    | _, Top -> Top
    | Safe, Safe -> Safe
    | Safe, t' -> t'
    | t, Safe -> t
    | Unresolved { var = var1; eval = eval1 },
      Unresolved { var = var2; eval = eval2 } ->
      let n = (List.len var1) in
      { var = var1@var2;
        eval = fun tl ->
          let t1,t2 = split tl n in
          transform_return (eval1 t1) (eval2 t2);
      }

  let is_not_safe = function Top -> true | Safe | Bot -> false

  let print ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top -> Format.fprintf ppf "top"
    | Safe -> Format.fprintf ppf "safe"
    | Unresolved { var; eval=_} ->
      let pp ppf l = Format.pp_print_list Var.print ppf l in
      Format.fprintf ppf "unresolved %a@,"
         pp var

  let rec eval t env =
    match t with
    | Bot -> Bot
    | Safe -> Safe
    | Top -> Top
    | Unresolved { var; eval } ->

      eval ()

end

(** Abstract value associated with each program location in a function. *)
module Value : sig
  type t =
    { nor : V.t;
          (** Property about
              all paths from this program location that may reach a Normal Return  *)
      exn : V.t;
          (** Property about all paths from this program point that may reach a Return with
          Exception *)
      div : V.t
          (** Property about all paths from this program point that may diverge.  *)
    }

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val top : t

  val bot : t

  val normal_return : t

  val exn_escape : t

  val diverges : t

  val safe : t

  val relaxed : t

  val print : Format.formatter -> t -> unit

  val transform : t -> t
  val transform_return : effect:V.t -> t -> t
  val transform_diverge : effect:V.t -> t -> t
end = struct
  (** Lifts V to triples  *)
  type t =
    { nor : V.t;
      exn : V.t;
      div : V.t
    }

  let bot = { nor = V.Bot; exn = V.Bot; div = V.Bot }

  let lessequal v1 v2 =
    V.lessequal v1.nor v2.nor && V.lessequal v1.exn v2.exn
    && V.lessequal v1.div v2.div

  let join v1 v2 =
    { nor = V.join v1.nor v2.nor;
      exn = V.join v1.exn v2.exn;
      div = V.join v1.div v2.div
    }

  let transform v =
    { nor = V.transform v.nor;
      exn = V.transform v.exn;
      div = V.transform v.div
    }

  let transform_return ~(effect : V.t) dst =
    match effect with
    | V.Bot -> Value.bot
    | V.Safe -> dst
    | V.Top -> Value.transform dst
    | V.Unresolved { b; s; t; var} ->
      { nor = V.transform_return dst.nor ~effect;
        exn = V.transform_return dst.exn ~effect;
        div = V.transform_return dst.div ~effect;
      }

  let transform_diverge ~(effect : V.t) (dst : Value.t) =
    let div = V.join effect dst.div in
    { dst with div }

  let normal_return = { bot with nor = V.Safe }

  let exn_escape = { bot with exn = V.Safe }

  let diverges = { bot with div = V.Safe }

  let safe = { nor = V.Safe; exn = V.Safe; div = V.Safe }

  let top = { nor = V.Top; exn = V.Top; div = V.Top }

  let relaxed = { nor = V.Safe; exn = V.Top; div = V.Top }

  let print ppf { nor; exn; div } =
    Format.fprintf ppf "{ nor=%a; exn=%a; div=%a }" V.print nor V.print exn
      V.print div
end

(**  Representation of user-provided annotations as abstract values *)
module Annotation : sig
  type t

  val find : Cmm.codegen_option list -> Cmm.property -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  val is_assume : t -> bool

  val report_error : exn -> Location.error option

  exception
    Invalid of
      { a : t;
        fun_name : string;
        property : Cmm.property
      }
end = struct
  (**
   ***************************************************************************
   *  [Strict] statically guarantees that all paths through the function satisfy
   *  all of the following conditions:
   *   - property holds on all primitive operations (e.g., no heap allocation)
   *   - no indirect calls (incl. no indirect tailcalls)
   *   - all direct calls (incl. tailcalls and probes) are to functions that
   *     satisfy the same conditions, i.e., they are [Strict].
   *
   *  [Relaxed] is the same as [Strict] on all paths that end in a normal return
   *  from a function, but no restrictions on diverging executions or
   *  on when a function returns with a [raise] with backtrace, which is treated
   *  as an error return (whereas [raise_no_trace] is treated as normal control flow
   *  and is subject to [Strict] requirements).
   *
   *****************************************************************************)

  type t =
    { strict : bool;  (** strict or relaxed? *)
      assume : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let expected_value t = if t.strict then Value.safe else Value.relaxed

  let is_assume t = t.assume

  let find codegen_options spec dbg =
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check { property; strict; assume; loc } when property = spec ->
            Some { strict; assume; loc }
          | Check _ | Reduce_code_size | No_CSE | Use_linscan_regalloc -> None)
        codegen_options
    in
    match a with
    | [] -> None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a"
        Debuginfo.print_compact dbg ()

  exception
    Invalid of
      { a : t;
        fun_name : string;
        property : Cmm.property
      }

  let print_error ppf t ~fun_name ~property =
    Format.fprintf ppf "Annotation check for %s%s failed on function %s"
      (Printcmm.property_to_string property)
      (if t.strict then " strict" else "")
      fun_name

  let report_error = function
    | Invalid { a; fun_name; property } ->
      Some
        (Location.error_of_printer ~loc:a.loc
           (print_error ~fun_name ~property)
           a)
    | _ -> None
end

module Func_info = struct
  type t =
    { name : string;  (** function name *)
      value : Value.t;  (** the result of the check *)
      annotation : Annotation.t option;
      (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
    }

  let create name value annotation=
    { name;
      value;
      annotation;
    }

  let print ~msg ppf t =
    let open Format in
    fprintf ppf "%s %s %a@." msg t.name Value.print t.value
end

module type Spec = sig
  (** Is the check enabled? *)
  val enabled : unit -> bool

  (** [get_value_opt f] returns the value recorded for function [f] in [Compilenv],
      either because the check passed or because of user-defined "assume" annotation.
      If [f] was compiled with checks disabled, returns None.
  *)
  val get_value_opt : string -> Value.t option

  (** [set_value f v] record the value of the function named [f] in [Compilenv]. *)
  val set_value : string -> Value.t -> unit

  (** Summary of target specific operations. *)
  val transform_specific : Arch.specific_operation -> Value.t

  val property : Cmm.property
end
(* CR-someday gyorsh: We may also want annotations on call sites, not only on
   functions. *)

(** Information about functions that we have seen so far in the current compilation
      unit. *)
module Unit_info : sig
  (** mutable state *)
  type t

  val create : unit -> t

  val reset : t -> unit

  val find_opt : t -> string -> Func_info.t option

  (** [recod t name v a] name must be in the current compilation unit,
      and not previously recorded.  *)
  val record : t -> string -> Value.t -> Annotation.t option -> unit

  val iter : t -> f:(Func_info.t -> unit) -> unit
end = struct
  (** map function name to the information about it *)
  type t = Func_info.t String.Tbl.t

  let create () = String.Tbl.create 17

  let reset t = String.Tbl.reset t

  let find_opt t name = String.Tbl.find_opt t name

  let iter t ~f = String.Tbl.iter (fun _ func_info -> f func_info) t

  let record t name value annotation =
    match String.Tbl.find_opt t name with
    | Some _ ->
      Misc.fatal_errorf "Duplicate symbol %s" name;
    | None ->
      let func_info = Func_info.create name value annotation in
      String.Tbl.replace t name func_info
end

(** Check one function. *)
module Analysis (S : Spec) : sig
  val fundecl :
    Mach.fundecl ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Format.formatter ->
    unit

  val record_unit : Unit_info.t -> Format.formatter -> unit
end = struct
  (** Information about the current function under analysis. *)
  type t =
    { ppf : Format.formatter;
      current_fun_name : string;
      future_funcnames : String.Set.t;
          (** functions defined later in the same compilation unit  *)
      unit_info : Unit_info.t;
      mutable approx : Value.t option
      (** Used for computing fixpoint for self calls. *)
    }

  let create ppf current_fun_name future_funcnames unit_info =
    { ppf;
      current_fun_name;
      future_funcnames;
      unit_info;
      approx = None;
    }

  let analysis_name = Printcmm.property_to_string S.property

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc Value.print v
        Debuginfo.print_compact dbg

  let report t v ~msg ~desc dbg =
    report' t.ppf v ~msg ~desc ~current_fun_name:t.current_fun_name dbg

  let report_fail t d desc dbg = report t d ~msg:"failed" ~desc dbg

  let is_future_funcname t callee = String.Set.mem callee t.future_funcnames

  let check t (r : Value.t) msg dbg =
    if V.is_not_safe r.nor then report_fail t r (msg ^ " nor") dbg;
    if V.is_not_safe r.exn then report_fail t r (msg ^ " exn") dbg;
    if V.is_not_safe r.div then report_fail t r (msg ^ " div") dbg;
    r

  let report_unit_info ~msg ppf unit_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Unit_info.iter unit_info ~f:(Func_info.print ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ppf ~msg func_info

  let record_unit unit_info ppf =
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        if (not (Annotation.is_assume a))
           && not
                (Value.lessequal func_info.value (Annotation.expected_value a))
        then
          (* CR gyorsh: we can add error recovering mode where we sets the
             expected value as the actual value and continue analysis of other
             functions. *)
          raise
            (Annotation.Invalid
               { a; fun_name = func_info.name; property = S.property }));
      report_func_info ~msg:"record" ppf func_info;
      S.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record

  let record_unit unit_info ppf =
    if S.enabled ()
    then
      Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
        (fun () -> record_unit unit_info ppf)

  (* [find_callee] returns the value associated with the callee. *)
  type callee =
    | Resolved of Value.t
    | Unresolved of Value.t

  let find_callee t callee =
    if is_future_funcname t callee then begin
      if String.equal callee t.current_fun_name then begin
        (* Self call. Summary is not computed yet. *)
        let approx =
        match t.approx with
          | None ->
            let v = Value.bot in
            t.approx <- Some v;
            v
          | Some approx -> approx
        in
        Unresolved approx
      end else
        (* Call is defined later in the current compilation unit.
           Summary of this callee is not yet computed,
           conservatively return Top. Won't be able to prove any recursive
           non-allocating functions. *)
        Unresolved Value.top
    end else begin
      match Unit_info.find_opt t.unit_info callee with
      | None ->
        (* Callee is not defined in the current compilation unit. *)
        (match S.get_value_opt callee with
         | None ->
           report t Value.top ~msg:"callee compiled without checks"
             ~desc:callee Debuginfo.none;
          Unresolved Value.top
         | Some v ->
           Resolved v)
      | Some callee_info ->
        (* Callee defined earlier in the same compilation unit. *)
        Resolved callee_info.value
    end

  let transform t ~next ~exn ~(effect : Value.t) desc dbg =
    let next = Value.transform_return ~effect:effect.nor next in
    let exn = Value.transform_return ~effect:effect.exn exn in
    report t next ~msg:"transform new next" ~desc dbg;
    report t exn ~msg:"transform new exn" ~desc dbg;
    let r = Value.join next exn in
    report t r ~msg:"transform join" ~desc dbg;
    let r = transform_diverge ~effect:effect.div r in
    report t r ~msg:"transform_call result" ~desc dbg;
    check t r desc dbg

  let transform_call t ~next ~exn callee ~desc dbg =
    report t next ~msg:"transform_call next" ~desc dbg;
    report t exn ~msg:"transform_call exn" ~desc dbg;
    let callee_value =
    match find_callee t callee with
      | Resolved v ->
        report t v ~msg:"resolved" ~desc dbg;
        v
      | Unresolved v ->
        report t v ~msg:"unresolved" ~desc dbg;
        v
    in
    transform t ~next ~exn ~effect:callee_value desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iload _ | Icompf _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Ivalueofint | Iintofvalue | Icsel _ | Iname_for_debugger _ ->
      assert (Mach.operation_is_pure op);
      assert (not (Mach.operation_can_raise op));
      next
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istore _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Iintop Icheckbound | Iintop_imm (Icheckbound, _) ->
      (* does not allocate even when it raises because checkbound exception is
         static. *)
      transform t ~next ~exn ~effect:Value.safe "checkbound" dbg
    | Ipoll _ (* CR gyorsh: poll points are considered allocations. *)
    | Ialloc { mode = Alloc_local; _ } ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ialloc { mode = Alloc_heap; _ } ->
      assert (not (Mach.operation_can_raise op));
      let r = Value.transform next in
      check t r "heap allocation" dbg
    | Iprobe { name; handler_code_sym } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      transform_call t ~next ~exn handler_code_sym ~desc dbg
    | Icall_ind -> transform t ~next ~exn ~effect:Value.top "indirect call" dbg
    | Itailcall_ind ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      transform t ~next:Value.normal_return ~exn:Value.exn_escape
        ~effect:Value.top "indirect tailcall" dbg
    | Icall_imm { func } ->
      transform_call t ~next ~exn func ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func } ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func
        ~desc:("direct tailcall to " ^ func)
        dbg
    | Iextcall { alloc = false; returns = true; _ } ->
      (* Sound to ignore [exn] because external call marked as noalloc does not
         raise. *)
      next
    | Iextcall { alloc = false; returns = false; _ } ->
      (* Sound to ignore [next] and [exn] because the call never returns or
         raises. *)
      Value.normal_return
    | Iextcall { func; alloc = true; _ } ->
      transform t ~next ~exn ~effect:Value.top ("external call to " ^ func) dbg
    | Ispecific s ->
      transform t ~next ~exn ~effect:(S.transform_specific s)
        "Arch.specific_operation" dbg

  module D = Dataflow.Backward ((Value : Dataflow.DOMAIN))

  let check_instr t body =
    let transfer (i : Mach.instruction) ~next ~exn =
      match i.desc with
      | Ireturn _ -> Value.normal_return
      | Iop op -> transform_operation t op ~next ~exn i.dbg
      | Iraise Raise_notrace ->
        (* [raise_notrace] is typically used for control flow, not for
           indicating an error. Therefore, we do not ignore any allocation on
           paths to it. The following conservatively assumes that normal and exn
           Returns are reachable. *)
        Value.join exn Value.safe
      | Iraise (Raise_reraise | Raise_regular) -> exn
      | Iend -> next
      | Iexit _ ->
        report t next ~msg:"transform" ~desc:"iexit" i.dbg;
        next
      | Iifthenelse _ | Iswitch _ -> next
      | Icatch (_rc, _ts, _, _body) ->
        report t next ~msg:"transform" ~desc:"catch" i.dbg;
        next
      | Itrywith (_body, (Regular | Delayed _), (_trap_stack, _handler)) ->
        report t next ~msg:"transform" ~desc:"try-with" i.dbg;
        next
    in
    (* By default, backward analysis does not check the property on paths that
       diverge (non-terminating loops that do not reach normal or exceptional
       return). All loops must go through an (Iexit label) instruction or a
       recursive function call. If (Iexit label) is not backward reachable from
       the function's Normal or Exceptional Return, either the loop diverges or
       the Iexit instruction is not reachable from function entry.

       To check divergent loops, the initial value of "div" component of all
       Iexit labels is set to "Safe" instead of "Bot". This is conservative with
       respect to non-recursive Icatch and Itrywith handlers. . *)
    let rec fixpoint () =
      let new_value =
        D.analyze ~exnescape:Value.exn_escape ~init_lbl:Value.diverges ~transfer
          body
        |> fst
      in
      match t.approx with
      | None -> new_value
      | Some approx ->
        if Value.lessequal new_value approx then
          approx
        else
          begin
            t.approx <- Some (Value.join new_value approx);
            fixpoint ()
          end
    in
    fixpoint ()

  let fundecl (f : Mach.fundecl) ~future_funcnames unit_info ppf =
    let check () =
      let fun_name = f.fun_name in
      let t = create ppf fun_name future_funcnames unit_info in
      let a = Annotation.find f.fun_codegen_options S.property f.fun_dbg in
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
        Unit_info.record unit_info fun_name expected_value None
      | None | Some _ ->
        report t Value.top ~msg:"assert" ~desc:"fundecl" f.fun_dbg;
        let res = check_instr t f.fun_body in
        report t res ~msg:"finished" ~desc:"fundecl" f.fun_dbg;
        report_unit_info ppf unit_info ~msg:"before join value";
        Unit_info.record unit_info fun_name res a;
        report_unit_info ppf unit_info ~msg:"after join value"
    in
    if S.enabled ()
    then Profile.record_call ~accumulate:true ("check " ^ analysis_name) check
end

(** Check that functions do not allocate on the heap (local allocations are ignored) *)
module Spec_zero_alloc : Spec = struct
  let property = Cmm.Zero_alloc

  let enabled () = !Flambda_backend_flags.zero_alloc_check

  (* Compact the mapping from function name to Value.t to reduce size of Checks
     in cmx and memory consumption Compilenv. Different components have
     different frequencies of Top/Bot. The most likely value is encoded as None
     (i.e., not stored). *)
  let encode_return (v : V.t) =
    match v with Top -> None | Safe -> Some true | Bot -> Some false

  let decode_return = function
    | None -> V.Top
    | Some true -> V.Safe
    | Some false -> V.Bot

  let encode_diverge (v : V.t) =
    match v with Top -> Some false | Safe -> Some true | Bot -> None

  let decode_diverge = function
    | None -> V.Bot
    | Some true -> V.Safe
    | Some false -> V.Top

  let set_value s (v : Value.t) =
    let checks = (Compilenv.current_unit_infos ()).ui_checks in
    let new_value : Checks.value =
      { nor = encode_return v.nor;
        exn = encode_return v.exn;
        div = encode_diverge v.div
      }
    in
    Checks.set_value checks s new_value

  let get_value_opt s =
    let checks = Compilenv.cached_checks in
    match Checks.get_value checks s with
    | None -> None
    | Some ({ nor; exn; div } : Checks.value) ->
      Some
        { Value.nor = decode_return nor;
          exn = decode_return exn;
          div = decode_diverge div
        }

  let transform_specific s =
    (* Conservatively assume that operation can return normally. *)
    let nor = if Arch.operation_allocates s then V.Top else V.Safe in
    let exn = if Arch.operation_can_raise s then nor else V.Bot in
    (* Assume that the operation does not diverge. *)
    let div = V.Bot in
    { Value.nor; exn; div }
end

module Check_zero_alloc = Analysis (Spec_zero_alloc)

(** Information about the current unit. *)
let unit_info = Unit_info.create ()

let fundecl ppf_dump ~future_funcnames fd =
  Check_zero_alloc.fundecl fd ~future_funcnames unit_info ppf_dump;
  fd

let reset_unit_info () = Unit_info.reset unit_info

let record_unit_info ppf_dump =
  Check_zero_alloc.record_unit unit_info ppf_dump;
  Compilenv.cache_checks (Compilenv.current_unit_infos ()).ui_checks

let () = Location.register_error_of_exn Annotation.report_error
