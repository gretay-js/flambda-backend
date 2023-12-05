type t =
  | No_assume
  | Assume of { strict: bool; never_returns_normally: bool; }

val compare : t -> t -> int
val equal : t -> t -> bool
val join : t -> t -> t
val meet : t -> t -> t
val to_string : t -> string




