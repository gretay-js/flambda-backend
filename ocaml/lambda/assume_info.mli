type t

val none : t
val create : strict:bool -> never_returns_normally:bool -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val join : t -> t -> t
val meet : t -> t -> t
val to_string : t -> string
val strict : t -> bool option
val never_returns_normally : t -> bool option
val is_none : t -> bool
