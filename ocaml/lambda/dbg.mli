type item = {
  dinfo_file : string;
  dinfo_line : int;
  dinfo_char_start : int;
  dinfo_char_end : int;
  dinfo_start_bol : int;
  dinfo_end_bol : int;
  dinfo_end_line : int;
  dinfo_scopes : Scoped_location.scopes;
}

type t = item list

(** [compare] and [hash] ignore [dinfo_scopes] field of item *)

val is_none : t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_list : t -> item list
val length : t -> int
val to_string : t -> string
val print_compact : Format.formatter -> t -> unit
val to_location : t -> Location.t
val none : t
