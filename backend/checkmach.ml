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

module Witness = struct
  type kind =
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo
        }
    | Indirect_call
    | Indirect_tailcall
    | Direct_call of { callee : string }
    | Direct_tailcall of { callee : string }
    | Extcall of { callee : string }
    | Arch_specific
    | Probe of
        { name : string;
          handler_code_sym : string
        }

  type t =
    { dbg : Debuginfo.t;
      kind : kind
    }

  let create dbg kind = { dbg; kind }

  let compare { dbg = dbg1; kind = kind1 } { dbg = dbg2; kind = kind2 } =
    (* compare by [dbg] first to print the errors in the order they appear in
       the source file. *)
    let c = Debuginfo.compare dbg1 dbg2 in
    if c <> 0 then c else Stdlib.compare kind1 kind2

  let print_kind ppf kind =
    let open Format in
    match kind with
    | Alloc { bytes; dbginfo = _ } -> fprintf ppf "allocation of %d bytes" bytes
    | Indirect_call -> fprintf ppf "indirect call"
    | Indirect_tailcall -> fprintf ppf "indirect tailcall"
    | Direct_call { callee } -> fprintf ppf "direct call %s" callee
    | Direct_tailcall { callee : string } ->
      fprintf ppf "direct tailcall %s" callee
    | Extcall { callee } -> fprintf ppf "external call to %s" callee
    | Arch_specific -> fprintf ppf "arch specific operation"
    | Probe { name; handler_code_sym } ->
      fprintf ppf "probe \"%s\" handler %s" name handler_code_sym

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a %a" print_kind kind Debuginfo.print_compact dbg
end

module Witnesses : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  val join : t -> t -> t

  val lessequal : t -> t -> bool

  val create : Witness.kind -> Debuginfo.t -> t

  val print : Format.formatter -> t -> unit

  val elements : t -> Witness.t list

  val compare : t -> t -> int

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  val simplify : components -> components
end = struct
  include Set.Make (Witness)

  (* CR gyorsh: consider using Flambda_backend_flags.checkmach_details_cutoff to
     limit the size of this set. The downside is that it won't get tested as
     much. Only keep witnesses for functions that need checking. *)
  let join = union

  let lessequal = subset

  let create kind dbg = singleton (Witness.create dbg kind)

  let iter t ~f = iter f t

  let print ppf t = Format.pp_print_seq Witness.print ppf (to_seq t)

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  let simplify { nor; exn; div } =
    { div =
        (* don't print diverge witnesses unless they are the only ones. *)
        (if is_empty nor && is_empty exn then div else empty);
      nor;
      (* only print the exn witnesses that are not also nor witnesses. *)
      exn = diff exn nor
    }
end

module Tag = struct
  type t =
    | N
    | E
    | D

  let compare = Stdlib.compare

  let print = function N -> "nor" | E -> "exn" | D -> "div"
end

module Var : sig
  type t

  val get : string -> Tag.t -> t

  val name : t -> string

  val tag : t -> Tag.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end = struct
  module T = struct
    type t =
      { name : string;
        tag : Tag.t
      }

    let compare { tag = tag1; name = name1 } { tag = tag2; name = name2 } =
      let c = String.compare name1 name2 in
      if c = 0 then Tag.compare tag1 tag2 else c
  end

  include T

  let name t = t.name

  let tag t = t.tag

  let get name tag = { name; tag }

  let print ppf { name; tag } =
    Format.fprintf ppf "%s.%s@," name (Tag.print tag)
end

(** Abstract value for each component of the domain. *)
module V : sig
  type u

  type t =
    | Top of Witnesses.t  (** Property may not hold on some paths. *)
    | Safe  (** Property holds on all paths.  *)
    | Bot  (** Not reachable. *)
    | Unresolved of u  (** [u] is normalized  *)

  (** order of the abstract domain  *)
  val lessequal : t -> t -> bool

  (** [equal] is structural equality on terms,
      not the order of the abstract domain. *)
  val equal : t -> t -> bool

  val join : t -> t -> t

  val transform : t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val diff_witnesses : expected:t -> actual:t -> Witnesses.t

  val get_witnesses : t -> Witnesses.t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  val unresolved : Var.t -> t

  val is_resolved : t -> bool

  val apply : t -> env:(Var.t -> t) -> t
end = struct
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot
    | Unresolved of u

  and u =
    | Const of t
    | Var of Var.t
    | Transform of u list
    | Join of u list

  let unresolved var = Unresolved (Var var)

  let is_resolved t =
    match t with Top _ | Safe | Bot -> true | Unresolved _ -> false

  let rec print ~witnesses ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top w ->
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
    | Safe -> Format.fprintf ppf "safe"
    | Unresolved u ->
      Format.fprintf ppf "unresolved %a@" (print_unresolved ~witnesses) u

  and print_unresolved ~witnesses ppf u =
    let pp ppf l = Format.pp_print_list (print_unresolved ~witnesses) ppf l in
    match u with
    | Join tl -> Format.fprintf ppf "(join %a)@," pp tl
    | Transform tl -> Format.fprintf ppf "(transform %a)@," pp tl
    | Var v -> Format.fprintf ppf "(var %a)@," Var.print v
    | Const (Unresolved _) -> assert false
    | Const ((Top _ | Safe | Bot) as t) ->
      Format.fprintf ppf "%a" (print ~witnesses) t

  (* [Unresolved] module groups functions that operate on [u]. Note that some of
     these functions return [t] to keep [u] in normal form, in cases when the
     value can be resolved as a result of an operation. *)
  module Unresolved : sig
    val equal : u -> u -> bool

    val join : u -> u -> t

    val transform : u -> u -> t

    val assert_normal_form : u -> unit
  end = struct
    (* structural comparison on terms, not the ordering on the abstract
       domain. *)
    let rec compare u1 u2 =
      match u1, u2 with
      | Const c1, Const c2 -> compare_t c1 c2
      | Const _, _ -> -1
      | _, Const _ -> 1
      | Var v1, Var v2 -> Var.compare v1 v2
      | Var _, _ -> -1
      | _, Var _ -> 1
      | Transform ul1, Transform ul2 -> List.compare compare ul1 ul2
      | Transform _, _ -> -1
      | _, Transform _ -> 1
      | Join ul1, Join ul2 -> List.compare compare ul1 ul2

    and compare_t t1 t2 =
      match t1, t2 with
      | Bot, Bot -> 0
      | Bot, _ -> -1
      | _, Bot -> 1
      | Safe, Safe -> 0
      | Safe, _ -> -1
      | _, Safe -> 1
      | Top w1, Top w2 -> Witnesses.compare w1 w2
      | Top _, Unresolved _ -> -1
      | Unresolved _, Top _ -> 1
      | Unresolved u1, Unresolved u2 -> compare u1 u2

    let equal u1 u2 = compare u1 u2 = 0

    module USet = Set.Make (struct
      type t = u

      let compare = compare
    end)

    let rec assert_normal_form u =
      match u with
      | Const _ -> assert false
      | Var _ -> ()
      | Transform ul -> assert_normal_form_transform ul
      | Join ul ->
        (* only (Const Safe), Var, or Transform in normal form, at least two
           elements. *)
        ul |> get_elements
        |> USet.iter (function
             | Const Safe -> ()
             | Var _ -> ()
             | Transform ul -> assert_normal_form_transform ul
             | Join _ -> assert false
             | Const (Top _ | Bot | Unresolved _) -> assert false)

    and assert_normal_form_transform ul =
      (* only (Const Top) or Var, at least two elements, sorted, no
         duplicates *)
      ul |> get_elements
      |> USet.iter (function
           | Const (Top _) -> ()
           | Var _ -> ()
           | Const (Bot | Safe | Unresolved _) | Transform _ | Join _ ->
             assert false)

    and get_elements ul =
      (* ensures: sorted, no duplicates, at least two elements *)
      let us = USet.of_list ul in
      assert (USet.cardinal us >= 2);
      assert (List.equal equal (USet.elements us) ul);
      us

    module N : sig
      val normalize : u -> t
    end = struct
      type acc =
        { us : USet.t;
          joins : USet.t;
          top : Witnesses.t option
        }

      let empty = { joins = USet.empty; us = USet.empty; top = None }

      (* CR-someday gyorsh: symmetry in handling join and transform, factor
         out? *)
      let rec normalize : u -> t =
       fun u ->
        if !Flambda_backend_flags.dump_checkmach
        then
          Format.fprintf Format.std_formatter "normalize: %a@."
            (print_unresolved ~witnesses:false)
            u;
        match u with
        | Const (Unresolved _) -> assert false
        | Const ((Top _ | Safe | Bot) as t) -> t
        | Var _ -> Unresolved u
        | Join ul -> normalize_join ul
        | Transform ul -> normalize_transform ul

      and flatten_transform acc u =
        match normalize u with
        | Safe -> acc
        | Bot -> { acc with us = USet.add (Const Bot) acc.us }
        | Top w ->
          let top =
            match acc.top with
            | None -> Some w
            | Some w' -> Some (Witnesses.join w w')
          in
          { acc with top }
        | Unresolved (Const _) -> assert false
        | Unresolved (Var _ as u) -> { acc with us = USet.add u acc.us }
        | Unresolved (Transform ul) -> List.fold_left flatten_transform acc ul
        | Unresolved (Join _ as u) -> { acc with joins = USet.add u acc.joins }

      and mk_transform res =
        assert (USet.is_empty res.joins);
        if USet.mem (Const Bot) res.us
        then Bot
        else
          let us =
            match res.top with
            | None -> res.us
            | Some w -> USet.add (Const (Top w)) res.us
          in
          match USet.elements us with
          | [] -> Safe
          | [u] -> (
            match u with
            | Const (Unresolved _ | Safe | Bot) | Join _ -> assert false
            | Const (Top _ as t) -> t
            | Var _ | Transform _ -> Unresolved u)
          | ul -> Unresolved (Transform ul)

      and distribute_transform_over_join acc =
        (* worst-case exponential in the number of variables *)
        match USet.choose_opt acc.joins with
        | None -> acc |> mk_transform
        | Some (Join ul as j) ->
          let acc = { acc with joins = USet.remove j acc.joins } in
          let f u = distribute_transform_over_join (flatten_transform acc u) in
          ul |> List.map f |> join_list
        | Some (Const _ | Transform _ | Var _) -> assert false

      and normalize_transform ul =
        let res = List.fold_left flatten_transform empty ul in
        (* optimize common cases *)
        if USet.mem (Const Bot) res.us
        then Bot
        else if USet.is_empty res.joins
        then mk_transform res
        else res |> distribute_transform_over_join

      and flatten_join acc u =
        match normalize u with
        | Bot -> acc
        | Safe -> { acc with us = USet.add (Const Safe) acc.us }
        | Top w ->
          let top =
            match acc.top with
            | None -> Some w
            | Some w' -> Some (Witnesses.join w w')
          in
          { acc with top }
        | Unresolved (Const _) -> assert false
        | Unresolved (Var _ as u) | Unresolved (Transform _ as u) ->
          { acc with us = USet.add u acc.us }
        | Unresolved (Join ul) ->
          let us = USet.(union acc.us (of_list ul)) in
          { acc with us }

      and normalize_join ul =
        let res = List.fold_left flatten_join empty ul in
        match res.top with
        | Some w -> Top w
        | None -> (
          match USet.elements res.us with
          | [] -> Bot
          | [u] -> (
            match u with
            | Const Safe -> Safe
            | Const (Top _ | Bot | Unresolved _) | Join _ -> assert false
            | (Var _ | Transform _) as u -> Unresolved u)
          | ul -> Unresolved (Join ul))

      and join_list tl =
        match tl with
        | [] -> Bot
        | [t] -> t
        | t :: tl -> join_t t (join_list tl)

      and join_t t1 t2 =
        (* CR gyorsh: fixme, copy of join from below, except doesn't
           normalize. *)
        let assert_not_join u =
          assert_normal_form u;
          match u with
          | Const _ -> assert false
          | Join _ -> assert false
          | Transform _ | Var _ -> ()
        in
        match t1, t2 with
        | Bot, Bot -> Bot
        | Safe, Safe -> Safe
        | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
        | Safe, Bot | Bot, Safe -> Safe
        | Top _, Bot | Top _, Safe -> t1
        | Bot, Top _ | Safe, Top _ -> t2
        | Top _, Unresolved _ -> t1
        | Unresolved _, Top _ -> t2
        | Bot, Unresolved _ -> t2
        | Unresolved _, Bot -> t1
        | Safe, Unresolved u | Unresolved u, Safe ->
          assert_not_join u;
          Unresolved (Join [Const Safe; u])
        | Unresolved u1, Unresolved u2 ->
          assert_not_join u1;
          assert_not_join u2;
          Unresolved (Join [u1; u2])
    end

    let join u1 u2 = Join [u1; u2] |> N.normalize

    let transform u1 u2 = Transform [u1; u2] |> N.normalize
  end

  let assert_normal_form t =
    match t with
    | Top _ | Safe | Bot -> ()
    | Unresolved u -> Unresolved.assert_normal_form u

  let join t1 t2 =
    assert_normal_form t1;
    assert_normal_form t2;
    match t1, t2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top _, Bot | Top _, Safe -> t1
    | Bot, Top _ | Safe, Top _ -> t2
    | Top _, Unresolved _ -> t1
    | Unresolved _, Top _ -> t2
    | Bot, Unresolved _ -> t2
    | Unresolved _, Bot -> t1
    | Safe, Unresolved u | Unresolved u, Safe -> Unresolved.join (Const Safe) u
    | Unresolved u1, Unresolved u2 -> Unresolved.join u1 u2

  let equal t1 t2 =
    match t1, t2 with
    | Top _, Top _ -> true
    | Safe, Safe -> true
    | Bot, Bot -> true
    | Unresolved u1, Unresolved u2 -> Unresolved.equal u1 u2
    | (Top _ | Safe | Bot | Unresolved _), _ -> false

  let lessequal t1 t2 =
    assert_normal_form t1;
    assert_normal_form t2;
    match t1, t2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top w1, Top w2 -> Witnesses.lessequal w1 w2
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false
    | Unresolved _, Top _ -> true
    | Top _, Unresolved _ -> false
    | Bot, Unresolved _ -> true
    | Unresolved _, Bot -> false
    | Unresolved _, Safe | Safe, Unresolved _ | Unresolved _, Unresolved _ ->
      equal (join t1 t2) t2

  (* Abstract transformer. Commutative and Associative.

     let transform t t' = if t = V.Bot || t' = V.Bot then V.Bot else (V.join t
     t')

     The implementation is an optimized version of the above definition that
     "inlines" and "specializes" join: efficently handle definitvie cases and
     preserve normal form of unresolved.

     Soundness (intuitively): If a return is unreachable from the program
     location immediately after the statement, or the statement does not return,
     then return is unreachable from the program location immediately before the
     statement. *)
  let transform t t' =
    assert_normal_form t;
    assert_normal_form t';
    match t, t' with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Safe, t' -> t'
    | t, Safe -> t
    | Top w, Top w' -> Top (Witnesses.join w w')
    | (Top _ as top), Unresolved u | Unresolved u, (Top _ as top) ->
      Unresolved.transform (Const top) u
    | Unresolved u, Unresolved u' -> Unresolved.transform u u'

  let rec apply t ~env =
    assert_normal_form t;
    match t with
    | Bot | Safe | Top _ -> t
    | Unresolved u -> apply_unresolved u ~env

  and apply_unresolved : u -> env:(Var.t -> t) -> t =
   fun u ~env ->
    match u with
    | Const (Unresolved _) -> assert false
    | Const ((Top _ | Bot | Safe) as t) -> t
    | Var v -> env v
    | Transform tl ->
      List.fold_left
        (fun acc u -> transform (apply_unresolved u ~env) acc)
        Safe tl
    | Join l ->
      List.fold_left (fun acc u -> join (apply_unresolved u ~env) acc) Bot l

  let rec replace_witnesses w t =
    match t with
    | Top _ -> Top w
    | Bot | Safe -> t
    | Unresolved u -> Unresolved (replace_witnesses_unresolved w u)

  and replace_witnesses_unresolved w u =
    match u with
    | Join tl -> Join (replace_witnesses_unresolved_list w tl)
    | Transform tl -> Transform (replace_witnesses_unresolved_list w tl)
    | Var _ -> u
    | Const t -> Const (replace_witnesses w t)

  and replace_witnesses_unresolved_list w tl =
    List.map (replace_witnesses_unresolved w) tl

  let get_witnesses t =
    match t with
    | Top w -> w
    | Bot | Safe -> Witnesses.empty
    | Unresolved _ -> assert false

  let diff_witnesses ~expected ~actual =
    (* If [actual] is Top and [expected] is not Top then return the [actual]
       witnesses. Otherwise, return empty. *)
    match actual, expected with
    | Unresolved _, _ | _, Unresolved _ -> assert false
    | Bot, Bot | Safe, Safe | Bot, Safe -> Witnesses.empty
    | Bot, Top w | Safe, Top w | Top _, Top w ->
      assert (Witnesses.is_empty w);
      Witnesses.empty
    | Safe, Bot -> Witnesses.empty
    | Top w, (Bot | Safe) -> w
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

  val top : Witnesses.t -> t

  val bot : t

  val normal_return : t

  val exn_escape : t

  val diverges : t

  val safe : t

  val relaxed : Witnesses.t -> t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  val transform : V.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val get_witnesses : t -> Witnesses.components

  val diff_witnesses : expected:t -> actual:t -> Witnesses.components

  val unresolved : string -> t

  val is_resolved : t -> bool

  val get_component : t -> Tag.t -> V.t

  val apply : t -> (Var.t -> V.t) -> t

  (** structural equality, as opposed to [lesseq] that is the order of the domain *)
  val equal : t -> t -> bool
end = struct
  (** Lifts V to triples  *)
  type t =
    { nor : V.t;
      exn : V.t;
      div : V.t
    }

  let bot = { nor = V.Bot; exn = V.Bot; div = V.Bot }

  let unresolved name =
    { nor = Var.get name Tag.N |> V.unresolved;
      exn = Var.get name Tag.E |> V.unresolved;
      div = Var.get name Tag.D |> V.unresolved
    }

  let is_resolved t =
    V.is_resolved t.nor && V.is_resolved t.exn && V.is_resolved t.div

  let equal t1 t2 =
    V.equal t1.nor t2.nor && V.equal t1.exn t2.exn && V.equal t1.div t2.div

  let get_component t (tag : Tag.t) =
    match tag with N -> t.nor | E -> t.exn | D -> t.div

  let apply t env =
    { nor = V.apply t.nor ~env;
      exn = V.apply t.exn ~env;
      div = V.apply t.div ~env
    }

  let lessequal v1 v2 =
    V.lessequal v1.nor v2.nor && V.lessequal v1.exn v2.exn
    && V.lessequal v1.div v2.div

  let join v1 v2 =
    { nor = V.join v1.nor v2.nor;
      exn = V.join v1.exn v2.exn;
      div = V.join v1.div v2.div
    }

  let transform effect v =
    { nor = V.transform effect v.nor;
      exn = V.transform effect v.exn;
      div = V.transform effect v.div
    }

  let replace_witnesses w t =
    { nor = V.replace_witnesses w t.nor;
      exn = V.replace_witnesses w t.exn;
      div = V.replace_witnesses w t.div
    }

  let diff_witnesses ~expected ~actual =
    { Witnesses.nor = V.diff_witnesses ~expected:expected.nor ~actual:actual.nor;
      Witnesses.exn = V.diff_witnesses ~expected:expected.exn ~actual:actual.exn;
      Witnesses.div = V.diff_witnesses ~expected:expected.div ~actual:actual.div
    }

  let get_witnesses t =
    { Witnesses.nor = V.get_witnesses t.nor;
      Witnesses.exn = V.get_witnesses t.exn;
      Witnesses.div = V.get_witnesses t.div
    }

  let normal_return = { bot with nor = V.Safe }

  let exn_escape = { bot with exn = V.Safe }

  let diverges = { bot with div = V.Safe }

  let safe = { nor = V.Safe; exn = V.Safe; div = V.Safe }

  let top w = { nor = V.Top w; exn = V.Top w; div = V.Top w }

  let relaxed w = { nor = V.Safe; exn = V.Top w; div = V.Top w }

  let print ~witnesses ppf { nor; exn; div } =
    let pp = V.print ~witnesses in
    Format.fprintf ppf "{ nor=%a; exn=%a; div=%a }" pp nor pp exn pp div
end

(**  Representation of user-provided annotations as abstract values *)
module Annotation : sig
  type t

  val get_loc : t -> Location.t

  val find :
    Cmm.codegen_option list -> Cmm.property -> string -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  (** [valid t value] returns true if and only if the [value] satisfies the annotation,
      i.e., [value] is less or equal to [expected_value a] when ignoring witnesses. *)

  val valid : t -> Value.t -> bool

  val diff_witnesses : t -> Value.t -> Witnesses.components

  val is_assume : t -> bool

  val is_strict : t -> bool

  val is_check_enabled :
    Cmm.codegen_option list -> string -> Debuginfo.t -> bool
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
      never_returns_normally : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let get_loc t = t.loc

  let expected_value t =
    let res = if t.strict then Value.safe else Value.relaxed Witnesses.empty in
    if t.never_returns_normally then { res with nor = V.Bot } else res

  let valid t v =
    (* Use Value.lessequal but ignore witnesses. *)
    let expected = expected_value t in
    let actual = Value.replace_witnesses Witnesses.empty v in
    Value.lessequal actual expected

  let diff_witnesses t v =
    let expected = expected_value t in
    Value.diff_witnesses ~actual:v ~expected

  let is_assume t = t.assume

  let is_strict t = t.strict

  let find codegen_options spec fun_name dbg =
    let ignore_assert_all = ref false in
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check { property; strict; loc } when property = spec ->
            Some { strict; assume = false; never_returns_normally = false; loc }
          | Assume { property; strict; never_returns_normally; loc }
            when property = spec ->
            Some { strict; assume = true; never_returns_normally; loc }
          | Ignore_assert_all property when property = spec ->
            ignore_assert_all := true;
            None
          | Ignore_assert_all _ | Check _ | Assume _ | Reduce_code_size | No_CSE
          | Use_linscan_regalloc ->
            None)
        codegen_options
    in
    match a with
    | [] ->
      if !Clflags.zero_alloc_check_assert_all && not !ignore_assert_all
      then
        Some
          { strict = false;
            assume = false;
            never_returns_normally = false;
            loc = Debuginfo.to_location dbg
          }
      else None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a for %s"
        Debuginfo.print_compact dbg fun_name ()

  let is_check_enabled codegen_options fun_name dbg =
    let is_enabled p =
      match find codegen_options p fun_name dbg with
      | None -> false
      | Some { assume; _ } -> not assume
    in
    List.exists is_enabled Cmm.all_properties
end

module Report : sig
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  val print : exn -> Location.error option
end = struct
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  let annotation_error ~property_name t =
    (* print location of the annotation, print function name as part of the
       message. *)
    let loc = Annotation.get_loc t.a in
    let print_annotated_fun ppf () =
      let scoped_name =
        t.fun_dbg |> Debuginfo.get_dbg |> Debuginfo.Dbg.to_list
        |> List.map (fun dbg ->
               Debuginfo.(Scoped_location.string_of_scopes dbg.dinfo_scopes))
        |> String.concat ","
      in
      Format.fprintf ppf "Annotation check for %s%s failed on function %s (%s)"
        property_name
        (if Annotation.is_strict t.a then " strict" else "")
        scoped_name t.fun_name
    in
    Location.error_of_printer ~loc print_annotated_fun ()

  let error_messages ~property_name t : Location.error list =
    let pp_inlined_dbg ppf dbg =
      (* Show inlined locations, if dbg has more than one item. The first item
         will be shown at the start of the error message. *)
      if Debuginfo.Dbg.length (Debuginfo.get_dbg dbg) > 1
      then Format.fprintf ppf " (%a)" Debuginfo.print_compact dbg
    in
    let print_comballoc dbg =
      match dbg with
      | [] | [_] -> "", []
      | alloc_dbginfo ->
        (* If one Ialloc is a result of combining multiple allocations, print
           details of each location. Currently, this cannot happen because
           checkmach is before comballoc. In the future, this may be done in the
           middle-end. *)
        let msg =
          Printf.sprintf " combining %d allocations below"
            (List.length alloc_dbginfo)
        in
        let details =
          List.map
            (fun (item : Debuginfo.alloc_dbginfo_item) ->
              let pp_alloc ppf =
                Format.fprintf ppf "allocate %d words%a" item.alloc_words
                  pp_inlined_dbg item.alloc_dbg
              in
              let aloc = Debuginfo.to_location item.alloc_dbg in
              Location.mkloc pp_alloc aloc)
            alloc_dbginfo
        in
        msg, details
    in
    let print_witness (w : Witness.t) ~component =
      (* print location of the witness, print witness description. *)
      let loc = Debuginfo.to_location w.dbg in
      let component_msg =
        if String.equal "" component
        then component
        else " on a path to " ^ component
      in
      let print_main_msg, sub =
        match w.kind with
        | Alloc { bytes = _; dbginfo } ->
          let comballoc_msg, sub = print_comballoc dbginfo in
          ( Format.dprintf "%a%s%s" Witness.print_kind w.kind component_msg
              comballoc_msg,
            sub )
        | Indirect_call | Indirect_tailcall | Direct_call _ | Direct_tailcall _
        | Extcall _ ->
          ( Format.dprintf "called function may allocate%s (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
        | Arch_specific | Probe _ ->
          ( Format.dprintf "expression may allocate%s@ (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
      in
      let pp ppf () =
        print_main_msg ppf;
        pp_inlined_dbg ppf w.dbg
      in
      Location.error_of_printer ~loc ~sub pp ()
    in
    let print_witnesses ws : Location.error list =
      let { Witnesses.nor; exn; div } = Witnesses.simplify ws in
      let f ws component =
        ws |> Witnesses.elements |> List.map (print_witness ~component)
      in
      List.concat [f div "diverge"; f nor ""; f exn "exceptional return"]
    in
    let details =
      match !Flambda_backend_flags.checkmach_details_cutoff with
      | No_details ->
        (* do not print witnesses. *)
        []
      | Keep_all -> print_witnesses t.witnesses
      | At_most cutoff ->
        let all = print_witnesses t.witnesses in
        if List.compare_length_with all cutoff <= 0
        then all
        else
          let result, _ = Misc.Stdlib.List.split_at cutoff all in
          result
    in
    annotation_error ~property_name t :: details

  let rec print_all msgs =
    (* Print all errors message in a compilation unit as separate messages to
       make editor integration easier. *)
    match msgs with
    | [] -> assert false
    | [last_error] ->
      (* Finally, raise Error with the last function. *)
      Some last_error
    | error :: tl ->
      Location.print_report Format.err_formatter error;
      print_all tl

  let print = function
    | Fail (reports, property) ->
      let property_name = Printcmm.property_to_string property in
      (* Sort by function's location. If debuginfo is missing, keep sorted by
         function name. *)
      let compare t1 t2 =
        let c = Debuginfo.compare t1.fun_dbg t2.fun_dbg in
        if not (Int.equal c 0)
        then c
        else String.compare t1.fun_name t2.fun_name
      in
      reports |> List.stable_sort compare
      |> List.concat_map (error_messages ~property_name)
      |> print_all
    | _ -> None
end

module Func_info : sig
  type t = private
    { name : string;  (** function name *)
      dbg : Debuginfo.t;  (** debug info associated with the function *)
      mutable value : Value.t;  (** the result of the check *)
      annotation : Annotation.t option;
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
      saved_fun : Mach.fundecl option
          (** If the function has callees that haven't been analyzed yet, keep function body
          so it can be reanalyzed when the callees are available.  *)
    }

  val create :
    string ->
    Value.t ->
    Debuginfo.t ->
    Annotation.t option ->
    Mach.fundecl option ->
    t

  val print : witnesses:bool -> msg:string -> Format.formatter -> t -> unit

  val update : t -> Value.t -> unit
end = struct
  type t =
    { name : string;  (** function name *)
      dbg : Debuginfo.t;  (** debug info associated with the function *)
      mutable value : Value.t;  (** the result of the check *)
      annotation : Annotation.t option;
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
      saved_fun : Mach.fundecl option
          (** If the function has callees that haven't been analyzed yet, keep function body
          so it can be reanalyzed when the callees are available.  *)
    }

  let create name value dbg annotation saved_fun =
    { name; dbg; value; annotation; saved_fun }

  let print ~witnesses ~msg ppf t =
    Format.fprintf ppf "%s %s %a@." msg t.name (Value.print ~witnesses) t.value

  let update t value = t.value <- value
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
  val transform_specific : Witnesses.t -> Arch.specific_operation -> Value.t

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

  (** [recod t name v dbg a] name must be in the current compilation unit,
      and not previously recorded.  *)
  val record :
    t ->
    string ->
    Value.t ->
    Debuginfo.t ->
    Annotation.t option ->
    Mach.fundecl option ->
    unit

  val iter : t -> f:(Func_info.t -> unit) -> unit

  val fold : t -> f:(Func_info.t -> 'a -> 'a) -> init:'a -> 'a
end = struct
  (** map function name to the information about it *)
  type t = Func_info.t String.Tbl.t

  let create () = String.Tbl.create 17

  let reset t = String.Tbl.reset t

  let find_opt t name = String.Tbl.find_opt t name

  let iter t ~f = String.Tbl.iter (fun _ func_info -> f func_info) t

  let fold t ~f ~init =
    String.Tbl.fold (fun _name func_info acc -> f func_info acc) t init

  let record t name value dbg annotation saved_fun =
    match String.Tbl.find_opt t name with
    | Some _ -> Misc.fatal_errorf "Duplicate symbol %s" name
    | None ->
      let func_info = Func_info.create name value dbg annotation saved_fun in
      String.Tbl.replace t name func_info
end

(** Ad-hoc statistics about fixpoint computation. *)
module Stats : sig
  type t

  val create : unit -> t

  val fixpoint_incr : t -> unit

  val selfrec : t -> int -> string -> unit

  val dataflow : t -> int -> string -> unit

  val print : Format.formatter -> Unit_info.t -> t -> unit
end = struct
  type s =
    { mutable total_functions : int;
      mutable unresolved_functions : int;
      mutable fixpoint_iterations : int;
      (* selfrec: recursive functions that do not have other unresolved
         callees *)
      mutable selfrec_max_iterations : int;
      mutable selfrec_max_fun_name : string;
      (* dataflow analysis iterations within a call to check_instr *)
      mutable dataflow_max_iterations : int;
      mutable dataflow_max_fun_name : string
    }

  type t = s option

  let create () =
    match
      !Flambda_backend_flags.dump_checkmach
      || !Flambda_backend_flags.dump_checkmach_stats
    with
    | false -> None
    | true ->
      Some
        { total_functions = 0;
          unresolved_functions = 0;
          fixpoint_iterations = 0;
          selfrec_max_iterations = 0;
          selfrec_max_fun_name = "";
          dataflow_max_iterations = 0;
          dataflow_max_fun_name = ""
        }

  let selfrec t c name =
    match t with
    | None -> ()
    | Some t ->
      if c > t.selfrec_max_iterations
      then (
        t.selfrec_max_iterations <- c;
        t.selfrec_max_fun_name <- name)

  let fixpoint_incr t =
    match t with
    | None -> ()
    | Some t -> t.fixpoint_iterations <- t.fixpoint_iterations + 1

  let dataflow t c name =
    match t with
    | None -> ()
    | Some t ->
      if c > t.dataflow_max_iterations
      then (
        t.dataflow_max_iterations <- c;
        t.dataflow_max_fun_name <- name)

  let print ppf unit_info t =
    match t with
    | None -> ()
    | Some t ->
      (* count selfrec and unresolved functions in this compilation unit *)
      Unit_info.iter unit_info ~f:(fun func_info ->
          t.total_functions <- t.total_functions + 1;
          if Option.is_some func_info.saved_fun
          then t.unresolved_functions <- t.unresolved_functions + 1);
      (* print all fields *)
      let h = "*** Checkmach stats" in
      Format.fprintf ppf "%s: Total number of functions: %d@." h
        t.total_functions;
      Format.fprintf ppf "%s: Unresolved functions: %d@." h
        t.unresolved_functions;
      Format.fprintf ppf "%s: Fixpoint iterations: %d@." h t.fixpoint_iterations;
      Format.fprintf ppf
        "%s: Self-recursive function with max iterations: %d,%s@." h
        t.selfrec_max_iterations t.selfrec_max_fun_name;
      Format.fprintf ppf "%s: Function with max iterations of dataflow: %d,%s@."
        h t.dataflow_max_iterations t.dataflow_max_fun_name;
      ()
end

let stats = ref (Stats.create ())

(** The analysis involved some fixed point computations.
    Termination: [Value.t] is a finite height domain and
    [transfer] is a monotone function w.r.t. [Value.lessequal] order.
*)
module Analysis (S : Spec) : sig
  (** Check one function. *)
  val fundecl :
    Mach.fundecl ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Format.formatter ->
    unit

  (** Resolve all function summaries, check them against user-provided assertions,
      and record the summaries in Compilenv to be saved in .cmx files *)
  val record_unit : Unit_info.t -> Format.formatter -> unit
end = struct
  (** Information about the current function under analysis. *)
  type t =
    { ppf : Format.formatter;
      current_fun_name : string;
      future_funcnames : String.Set.t;
      mutable approx : Value.t option;
          (** Used for computing for self calls. *)
      mutable unresolved : bool;
          (** the current function contains calls to other unresolved functions (not including self calls) *)
      unit_info : Unit_info.t;  (** must be the current compilation unit.  *)
      keep_witnesses : bool
    }

  let should_keep_witnesses keep =
    match !Flambda_backend_flags.checkmach_details_cutoff with
    | Keep_all -> true
    | No_details -> false
    | At_most _ -> keep

  let create ppf current_fun_name future_funcnames unit_info approx annot =
    let keep_witnesses = should_keep_witnesses (Option.is_some annot) in
    { ppf;
      current_fun_name;
      future_funcnames;
      approx;
      unresolved = false;
      unit_info;
      keep_witnesses
    }

  let analysis_name = Printcmm.property_to_string S.property

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc
        (Value.print ~witnesses:true)
        v Debuginfo.print_compact dbg

  let report t v ~msg ~desc dbg =
    report' t.ppf v ~msg ~desc ~current_fun_name:t.current_fun_name dbg

  let is_future_funcname t callee = String.Set.mem callee t.future_funcnames

  let report_unit_info ppf unit_info ~msg =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Unit_info.iter unit_info ~f:(Func_info.print ~witnesses:true ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ~witnesses:true ppf ~msg func_info

  let record_unit ppf unit_info =
    let errors = ref [] in
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        Builtin_attributes.mark_property_checked analysis_name
          (Annotation.get_loc a);
        if (not (Annotation.is_assume a))
           && S.enabled ()
           && not (Annotation.valid a func_info.value)
        then
          (* CR-soon gyorsh: keeping track of all the witnesses until the end of
             the compilation unit will be expensive. For functions that do not
             have any dependencies, we can check annotations earlier, as soon as
             the function is analyzed, or as soon as its dependencies are
             resolved, print the error, and remove the witnesses from the stored
             values. *)
          (* CR gyorsh: we can add error recovering mode where we sets the
             expected value as the actual value and continue analysis of other
             functions. *)
          let witnesses = Annotation.diff_witnesses a func_info.value in
          errors
            := { Report.a;
                 fun_name = func_info.name;
                 fun_dbg = func_info.dbg;
                 witnesses
               }
               :: !errors);
      report_func_info ~msg:"record" ppf func_info;
      S.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record;
    match !errors with
    | [] -> ()
    | errors -> raise (Report.Fail (errors, S.property))

  let[@inline always] create_witnesses t kind dbg =
    if t.keep_witnesses then Witnesses.create kind dbg else Witnesses.empty

  (* [find_callee] returns the value associated with the callee. *)
  let find_callee t callee ~desc dbg w =
    let return ~msg v =
      report t v ~msg ~desc dbg;
      (* Abstract witnesses of a call to the single witness for the callee name.
         Summary of tailcall self won't be affected because it is not set to Top
         by [find_callee]. *)
      Value.replace_witnesses w v
    in
    let unresolved v reason =
      let msg = Printf.sprintf "unresolved %s (%s)" callee reason in
      return ~msg v
    in
    let resolved v =
      let msg = Printf.sprintf "resolved  %s" callee in
      return ~msg v
    in
    if is_future_funcname t callee
    then
      if String.equal callee t.current_fun_name
      then
        (* Self call. *)
        match t.approx with
        | None ->
          (* Summary is not computed yet. Conservative. *)
          let v = Value.safe in
          t.approx <- Some v;
          unresolved (Value.unresolved callee) "self-call init"
        | Some approx -> unresolved approx "self-call approx"
      else
        ((* Call is defined later in the current compilation unit. Summary of
            this callee is not yet computed. *)
         let res =
           if !Flambda_backend_flags.disable_precise_checkmach
           then
             (* Conservatively return Top. Won't be able to prove any recursive
                functions as non-allocating. *)
             Value.top w
           else (
             t.unresolved <- true;
             Value.unresolved callee)
         in
         unresolved res)
          "conservative handling of forward or recursive call\nor tailcall"
    else
      (* CR gyorsh: unresolved case here is impossible in the conservative
         analysis because all previous functions have been conservatively
         resolved.*)
      match Unit_info.find_opt t.unit_info callee with
      | None -> (
        (* Callee is not defined in the current compilation unit. *)
        match S.get_value_opt callee with
        | None ->
          unresolved (Value.top w)
            "(missing summary: callee compiled without checks)"
        | Some v -> resolved v)
      | Some callee_info -> (
        (* Callee defined earlier in the same compilation unit. *)
        match callee_info.saved_fun with
        | None -> resolved callee_info.value
        | Some _ ->
          (* callee was unresolved, mark caller as unresolved *)
          t.unresolved <- true;
          unresolved callee_info.value "unresolved callee")

  let transform_return ~(effect : V.t) dst =
    (* Instead of calling [Value.transform] directly, first check for trivial
       cases to avoid reallocating [dst]. *)
    match effect with
    | V.Bot -> Value.bot
    | V.Safe -> dst
    | V.Top _ -> Value.transform effect dst
    | V.Unresolved _ -> Value.transform effect dst

  let transform_diverge ~(effect : V.t) (dst : Value.t) =
    let div = V.join effect dst.div in
    { dst with div }

  let transform t ~next ~exn ~(effect : Value.t) desc dbg =
    let next = transform_return ~effect:effect.nor next in
    let exn = transform_return ~effect:effect.exn exn in
    report t next ~msg:"transform new next" ~desc dbg;
    report t exn ~msg:"transform new exn" ~desc dbg;
    let r = Value.join next exn in
    report t r ~msg:"transform join" ~desc dbg;
    let r = transform_diverge ~effect:effect.div r in
    report t r ~msg:"transform result" ~desc dbg;
    r

  let transform_top t ~next ~exn w desc dbg =
    let effect =
      if Debuginfo.assume_zero_alloc dbg then Value.relaxed w else Value.top w
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_call t ~next ~exn callee w ~desc dbg =
    report t next ~msg:"transform_call next" ~desc dbg;
    report t exn ~msg:"transform_call exn" ~desc dbg;
    let effect =
      if Debuginfo.assume_zero_alloc dbg
      then Value.relaxed w
      else find_callee t callee ~desc dbg w
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ | Iconst_symbol _
    | Iconst_vec128 _ | Iload _ | Icompf _ | Inegf | Iabsf | Iaddf | Isubf
    | Imulf | Idivf | Ifloatofint | Iintoffloat | Ivectorcast _ | Iscalarcast _
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Icsel _ ->
      assert (Mach.operation_is_pure op);
      assert (not (Mach.operation_can_raise op));
      next
    | Iname_for_debugger _ | Ivalueofint | Iintofvalue ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istore _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ipoll _
    (* Ignore poll points even though they may trigger an allocations, because
       otherwise all loops would be considered allocating when poll insertion is
       enabled. [@poll error] should be used instead. *)
    | Ialloc { mode = Alloc_local; _ } ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ialloc { mode = Alloc_heap; bytes; dbginfo } ->
      assert (not (Mach.operation_can_raise op));
      if Debuginfo.assume_zero_alloc dbg
      then next
      else
        let w = create_witnesses t (Alloc { bytes; dbginfo }) dbg in
        transform_top t ~next ~exn:Value.bot w "heap allocation" dbg
    | Iprobe { name; handler_code_sym; enabled_at_init = __ } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      let w = create_witnesses t (Probe { name; handler_code_sym }) dbg in
      transform_call t ~next ~exn handler_code_sym w ~desc dbg
    | Icall_ind ->
      let w = create_witnesses t Indirect_call dbg in
      transform_top t ~next ~exn w "indirect call" dbg
    | Itailcall_ind ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t Indirect_tailcall dbg in
      transform_top t ~next:Value.normal_return ~exn:Value.exn_escape w
        "indirect tailcall" dbg
    | Icall_imm { func = { sym_name = func; _ } } ->
      let w = create_witnesses t (Direct_call { callee = func }) dbg in
      transform_call t ~next ~exn func w ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func = { sym_name = func; _ } } ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t (Direct_tailcall { callee = func }) dbg in
      transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func w
        ~desc:("direct tailcall to " ^ func)
        dbg
    | Iextcall { alloc = false; returns = true; _ } ->
      (* Sound to ignore [exn] because external call marked as noalloc does not
         raise. *)
      next
    | Iextcall { alloc = false; returns = false; _ } ->
      (* Sound to ignore [next] and [exn] because the call never returns or
         raises. *)
      Value.bot
    | Iextcall { func; alloc = true; _ } ->
      let w = create_witnesses t (Extcall { callee = func }) dbg in
      transform_top t ~next ~exn w ("external call to " ^ func) dbg
    | Ispecific s ->
      let effect =
        let w = create_witnesses t Arch_specific dbg in
        if Debuginfo.assume_zero_alloc dbg
        then
          (* Conservatively assume that operation can return normally. *)
          let nor = V.Safe in
          let exn = if Arch.operation_can_raise s then V.Top w else V.Bot in
          (* Assume that the operation does not diverge. *)
          let div = V.Bot in
          { Value.nor; exn; div }
        else S.transform_specific w s
      in
      transform t ~next ~exn ~effect "Arch.specific_operation" dbg
    | Idls_get -> Misc.fatal_error "Idls_get not supported"

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
      | Itrywith (_body, _, (_trap_stack, _handler)) ->
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
       Iexit labels of recurisve Icatch handlers is set to "Safe" instead of
       "Bot". *)
    let stats counter = Stats.dataflow !stats counter t.current_fun_name in
    D.analyze ~exnescape:Value.exn_escape ~init_rc_lbl:Value.diverges ~transfer
      ~stats body
    |> fst

  let analyze_body t (fd : Mach.fundecl) =
    let counter = ref 0 in
    let rec fixpoint () =
      let new_value = check_instr t fd.fun_body in
      match t.approx with
      | None -> new_value
      | Some approx ->
        (* Fixpoint here is only for the common case of "self" recursive
           functions that do not have other unresolved dependencies. Other cases
           will be recomputed simultaneously at the end of the compilation
           unit. *)
        if t.unresolved
        then new_value
        else (
          incr counter;
          if Value.lessequal new_value approx
          then approx
          else (
            t.approx <- Some (Value.join new_value approx);
            fixpoint ()))
    in
    if !Flambda_backend_flags.dump_checkmach
    then Printmach.phase "Checkmach" t.ppf fd;
    let res = fixpoint () in
    Stats.selfrec !stats !counter t.current_fun_name;
    res

  let fixpoint_mach ppf unit_info =
    report_unit_info ppf unit_info ~msg:"before fixpoint";
    (* CR gyorsh: this is a really dumb iteration strategy. *)
    let change = ref true in
    let analyze_func (func_info : Func_info.t) =
      match func_info.saved_fun with
      | None -> ()
      | Some fd ->
        let t =
          create ppf func_info.name String.Set.empty unit_info None
            func_info.annotation
        in
        let new_value = analyze_body t fd in
        if not (Value.lessequal new_value func_info.value)
        then (
          change := true;
          report t new_value ~msg:"update" ~desc:"fixpoint" func_info.dbg;
          Func_info.update func_info new_value)
    in
    while !change do
      change := false;
      Unit_info.iter unit_info ~f:analyze_func;
      Stats.fixpoint_incr !stats;
      report_unit_info ppf unit_info ~msg:"computing fixpoint"
    done

  module Env : sig
    type t

    val empty : t

    val add : Func_info.t -> Value.t -> t -> t

    val get_value : string -> t -> Value.t

    val iter : t -> f:(Func_info.t -> Value.t -> unit) -> unit

    val map : t -> f:(Func_info.t -> Value.t -> Value.t) -> t
  end = struct
    type data =
      { func_info : Func_info.t;
        approx : Value.t
      }

    type t = data String.Map.t

    let empty = String.Map.empty

    let add (func_info : Func_info.t) approx t =
      let d = { func_info; approx } in
      String.Map.add func_info.name d t

    let get_value name t =
      let d = String.Map.find name t in
      d.approx

    let map t ~f =
      String.Map.map (fun d -> { d with approx = f d.func_info d.approx }) t

    let iter t ~f = String.Map.iter (fun _name d -> f d.func_info d.approx) t
  end

  let fixpoint_symbolic ppf unit_info =
    report_unit_info ppf unit_info ~msg:"before fixpoint";
    let env =
      (* initialize [env] with Bot for all functions. *)
      Unit_info.fold unit_info ~init:Env.empty ~f:(fun func_info env ->
          Env.add func_info Value.bot env)
    in
    let lookup env var =
      let v = Env.get_value (Var.name var) env in
      Value.get_component v (Var.tag var)
    in
    let rec loop env =
      let changed = ref false in
      let env' =
        Env.map
          ~f:(fun func_info v ->
            let v' = Value.apply func_info.value (lookup env) in
            assert (Value.is_resolved v');
            changed := not (Value.lessequal v' v);
            v')
          env
      in
      report_unit_info ppf unit_info ~msg:"computing fixpoint";
      if !changed then loop env' else env'
    in
    let env = loop env in
    env

  let _fixpoint_symbolic_optimized _ppf _unit_info =
    Misc.fatal_error "not implemented"

  let check_fixpoint ppf unit_info env =
    fixpoint_mach ppf unit_info;
    (* let opt_env = fixpoint_symbolic_optimized ppf unit_info in *)
    let pv ppf v = Value.print ppf ~witnesses:false v in
    Unit_info.iter unit_info ~f:(fun func_info ->
        let name = func_info.name in
        let expected_value = Env.get_value name env in
        if not (Value.is_resolved expected_value)
        then
          Misc.fatal_errorf "expected value for function %s is not resolved"
            name;
        if not (Value.is_resolved func_info.value)
        then
          Misc.fatal_errorf "recorded value for function %s is not resolved"
            name;
        if not (Value.equal expected_value func_info.value)
        then
          Misc.fatal_errorf "Expected value %a for function %s, recorded %a" pv
            expected_value name pv func_info.value;
        (* let actual_value = Env.get_value name actual in
         * if not (Value.is_resolved actual_value)
         * then
         *   Misc.fatal_errorf "actual value for function %s is not resolved" name;
         * if not (Value.equal expected_value actual_value)
         * then
         *   Misc.fatal_errorf
         *     "Recorded value %a for function %s does not match expected %a" pv
         *     expected_value name pv actual_value; *)
        ())

  let debug = true

  (* CR gyorsh: do we need join in the fixpoint computation or is the function
     body analysis/summary already monotone? *)
  let fixpoint ppf unit_info =
    let env = fixpoint_symbolic ppf unit_info in
    if debug then check_fixpoint ppf unit_info env;
    Env.iter env ~f:(fun func_info v -> Func_info.update func_info v)

  let record_unit unit_info ppf =
    Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
      (fun () ->
        fixpoint ppf unit_info;
        Stats.print ppf unit_info !stats;
        record_unit ppf unit_info)

  let fundecl (f : Mach.fundecl) ~future_funcnames unit_info ppf =
    let check () =
      let fun_name = f.fun_name in
      let a =
        Annotation.find f.fun_codegen_options S.property fun_name f.fun_dbg
      in
      let t = create ppf fun_name future_funcnames unit_info None a in
      let really_check () =
        let res = analyze_body t f in
        let saved_fun = if t.unresolved then Some f else None in
        report t res ~msg:"finished" ~desc:"fundecl" f.fun_dbg;
        if (not t.keep_witnesses) && Value.is_resolved res
        then (
          let { Witnesses.nor; exn; div } = Value.get_witnesses res in
          assert (Witnesses.is_empty nor);
          assert (Witnesses.is_empty exn);
          assert (Witnesses.is_empty div));
        Unit_info.record unit_info fun_name res f.fun_dbg a saved_fun;
        report_unit_info ppf unit_info ~msg:"after record"
      in
      let really_check () =
        if !Flambda_backend_flags.disable_checkmach
        then
          (* Do not analyze the body of the function, conservatively assume that
             the summary is top. *)
          Unit_info.record unit_info fun_name
            (Value.top Witnesses.empty)
            f.fun_dbg a None
        else really_check ()
      in
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
        Unit_info.record unit_info fun_name expected_value f.fun_dbg None None
      | None -> really_check ()
      | Some a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assert" ~desc:"fundecl" f.fun_dbg;
        (* Only keep witnesses for functions that need checking. *)
        really_check ()
    in
    Profile.record_call ~accumulate:true ("check " ^ analysis_name) check
end

(** Check that functions do not allocate on the heap (local allocations are ignored) *)
module Spec_zero_alloc : Spec = struct
  let property = Cmm.Zero_alloc

  let enabled () =
    (* Checkmach no longer distinguishes between opt and default checks. *)
    match !Clflags.zero_alloc_check with
    | No_check -> false
    | Check_default -> true
    | Check_all -> true
    | Check_opt_only -> true

  (* Compact the mapping from function name to Value.t to reduce size of Checks
     in cmx and memory consumption Compilenv. Different components have
     different frequencies of Top/Bot. The most likely value is encoded as None
     (i.e., not stored). *)
  let encode (v : V.t) =
    match v with
    | Top _ -> 0
    | Safe -> 1
    | Bot -> 2
    | Unresolved _ -> assert false

  (* Witnesses are not used across functions and not stored in cmx. Witnesses
     that appear in a function's summary are only used for error messages about
     that function, not about its callers. Witnesses from the summary of a
     callee are ignored, and replaced by the name of the callee. *)
  let decoded_witness = Witnesses.empty

  let decode = function
    | 0 -> V.Top decoded_witness
    | 1 -> V.Safe
    | 2 -> V.Bot
    | n -> Misc.fatal_errorf "Checkmach cannot decode %d" n

  let encode (v : Value.t) : Checks.value =
    let c = (encode v.div lsl 4) lor (encode v.exn lsl 2) lor encode v.nor in
    if c = 0 then None else Some c

  let decode : Checks.value -> Value.t = function
    | None -> Value.top decoded_witness
    | Some d ->
      if d = 0 then Misc.fatal_error "Checkmach unexpected 0 encoding";
      let nor = decode (d land 3) in
      let exn = decode ((d lsr 2) land 3) in
      let div = decode ((d lsr 4) land 3) in
      { nor; exn; div }

  let set_value s (v : Value.t) =
    let checks = (Compilenv.current_unit_infos ()).ui_checks in
    Checks.set_value checks s (encode v)

  let get_value_opt s =
    let checks = Compilenv.cached_checks in
    match Checks.get_value checks s with
    | None -> None
    | Some (c : Checks.value) -> Some (decode c)

  let transform_specific w s =
    (* Conservatively assume that operation can return normally. *)
    let nor = if Arch.operation_allocates s then V.Top w else V.Safe in
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

let reset_unit_info () =
  Unit_info.reset unit_info;
  stats := Stats.create ()

let record_unit_info ppf_dump =
  Check_zero_alloc.record_unit unit_info ppf_dump;
  Compilenv.cache_checks (Compilenv.current_unit_infos ()).ui_checks

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit

let iter_witnesses f =
  Unit_info.iter unit_info ~f:(fun func_info ->
      f func_info.name
        (Value.get_witnesses func_info.value |> Witnesses.simplify))

let () = Location.register_error_of_exn Report.print

let is_check_enabled codegen_options fun_name dbg =
  Annotation.is_check_enabled codegen_options fun_name dbg
