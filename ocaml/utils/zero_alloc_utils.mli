(** When the check fails, [Witness.t] represents an instruction that does
    not satisfy the property. *)
module Witness : sig
  module Kind : sig
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

    val print : Format.formatter -> t -> unit

    val get_alloc_dbginfo : t -> alloc_dbginfo option
  end

  type t = private
    { dbg : Dbg.t;
      kind : Kind.t
    }

  val create : Dbg.t -> Kind.t -> t

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
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

  (** The witnesses are classified into which path they may appear on. If a witness
      appears on both a path to a normal and an excpetional return, it will only appear in
      [nor] component. *)
  type components =
    { nor : t;  (** on a path from function entry to a normal return  *)
      exn : t;  (** on a path from function entry to an exceptionall return  *)
      div : t  (** on a path from function entry that may diverge *)
    }

  val simplify : components -> components
end

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
end

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
end
