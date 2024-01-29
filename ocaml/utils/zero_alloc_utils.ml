module Witness = struct
  module Kind = struct
    type alloc_dbginfo_item =
      { alloc_words : int;
        alloc_dbg : Dbg.t
      }

    type alloc_dbginfo = alloc_dbginfo_item list

    type t =
      | Alloc of
          { bytes : int;
            dbginfo : alloc_dbginfo
          }
      | Indirect_call
      | Indirect_tailcall
      | Direct_call of { callee : string }
      | Direct_tailcall of { callee : string }
      | Missing_summary of { callee : string }
      | Forward_call of { callee : string }
      | Extcall of { callee : string }
      | Arch_specific
      | Probe of
          { name : string;
            handler_code_sym : string
          }

    let get_alloc_dbginfo kind =
      match kind with
      | Alloc { bytes = _; dbginfo } -> Some dbginfo
      | Indirect_call | Indirect_tailcall | Direct_call _ | Direct_tailcall _
      | Missing_summary _ | Forward_call _ | Extcall _ | Arch_specific | Probe _
        ->
        None

    let print ppf kind =
      let open Format in
      match kind with
      | Alloc { bytes; dbginfo = _ } ->
        fprintf ppf "allocation of %d bytes" bytes
      | Indirect_call -> fprintf ppf "indirect call"
      | Indirect_tailcall -> fprintf ppf "indirect tailcall"
      | Direct_call { callee } -> fprintf ppf "direct call %s" callee
      | Direct_tailcall { callee : string } ->
        fprintf ppf "direct tailcall %s" callee
      | Missing_summary { callee } ->
        fprintf ppf "missing summary for %s" callee
      | Forward_call { callee } ->
        fprintf ppf "foward call or tailcall (conservatively handled) %s" callee
      | Extcall { callee } -> fprintf ppf "external call to %s" callee
      | Arch_specific -> fprintf ppf "arch specific operation"
      | Probe { name; handler_code_sym } ->
        fprintf ppf "probe %s handler %s" name handler_code_sym
  end

  type t =
    { dbg : Dbg.t;
      kind : Kind.t
    }

  let create dbg kind = { dbg; kind }

  let compare { dbg = dbg1; kind = kind1 } { dbg = dbg2; kind = kind2 } =
    (* compare by [dbg] first to print the errors in the order they appear in
       the source file. *)
    let c = Dbg.compare dbg1 dbg2 in
    if c <> 0 then c else Stdlib.compare kind1 kind2

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a %a" Kind.print kind Dbg.print_compact dbg
end

module Witnesses : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  val join : t -> t -> t

  val create : Witness.Kind.t -> Dbg.t -> t

  val print : Format.formatter -> t -> unit

  val elements : t -> Witness.t list

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

(** Abstract value for each component of the domain. *)
module V : sig
  type t =
    | Top of Witnesses.t  (** Property may not hold on some paths. *)
    | Safe  (** Property holds on all paths.  *)
    | Bot  (** Not reachable. *)

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val transform : Witnesses.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val diff_witnesses : expected:t -> actual:t -> Witnesses.t

  val get_witnesses : t -> Witnesses.t

  val is_not_safe : t -> bool

  val print : witnesses:bool -> Format.formatter -> t -> unit
end = struct
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot

  let join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top w1, Bot | Top w1, Safe | Bot, Top w1 | Safe, Top w1 -> Top w1

  let lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top _, Top _ -> true
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false

  (** abstract transformer (backward analysis) for a statement that violates the property
      but doesn't alter control flow. *)
  let transform w = function
    | Bot ->
      (* if a return is unreachable from the program location immediately after
         the statement, then return is unreachable from the program location
         immediately before the statement. *)
      Bot
    | Safe -> Top w
    | Top w' -> Top (Witnesses.join w w')

  let replace_witnesses w t = match t with Top _ -> Top w | Bot | Safe -> t

  let get_witnesses t =
    match t with Top w -> w | Bot | Safe -> Witnesses.empty

  let diff_witnesses ~expected ~actual =
    if lessequal actual expected
    then Witnesses.empty
    else (
      assert (expected = Safe);
      match actual with Bot | Safe -> assert false | Top w -> w)

  let is_not_safe = function Top _ -> true | Safe | Bot -> false

  let print ~witnesses ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top w ->
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
    | Safe -> Format.fprintf ppf "safe"
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

  val transform : Witnesses.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val get_witnesses : t -> Witnesses.components

  val diff_witnesses : expected:t -> actual:t -> Witnesses.components
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

  let transform w v =
    { nor = V.transform w v.nor;
      exn = V.transform w v.exn;
      div = V.transform w v.div
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
