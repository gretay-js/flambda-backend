module type WS = sig
  type t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module type Component = sig
  type t

  type witnesses

  val top : witnesses -> t

  val safe : t

  val bot : t

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val compare : t -> t -> int

  val print : witnesses:bool -> Format.formatter -> t -> unit
end

module Make_component (Witnesses : WS) = struct
  (* keep in sync with "resolved" values in Checkmach. *)
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot

  let bot = Bot

  let top w = Top w

  let safe = Safe

  let join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top w1, Bot | Top w1, Safe | Bot, Top w1 | Safe, Top w1 -> Top w1

  let meet c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.meet w1 w2)
    | Safe, Bot | Bot, Safe -> Bot
    | Top _, Bot | Bot, Top _ -> Bot
    | Top _, Safe | Safe, Top _ -> Safe

  let lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top w1, Top w2 -> Witnesses.lessequal w1 w2
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false

  let compare t1 t2 =
    match t1, t2 with
    | Bot, Bot -> 0
    | Safe, Safe -> 0
    | Top w1, Top w2 -> Witnesses.compare w1 w2
    | Bot, (Safe | Top _) -> -1
    | (Safe | Top _), Bot -> 1
    | Safe, Top _ -> -1
    | Top _, Safe -> 1

  let print ~witnesses ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top w ->
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
    | Safe -> Format.fprintf ppf "safe"
end

module Make_value
    (Witnesses : WS)
    (V : Component with type witnesses := Witnesses.t) =
struct
  (** Lifts V to triples  *)
  type t =
    { nor : V.t;
      exn : V.t;
      div : V.t
    }

  let bot = { nor = V.bot; exn = V.bot; div = V.bot }

  let lessequal v1 v2 =
    V.lessequal v1.nor v2.nor && V.lessequal v1.exn v2.exn
    && V.lessequal v1.div v2.div

  let join v1 v2 =
    { nor = V.join v1.nor v2.nor;
      exn = V.join v1.exn v2.exn;
      div = V.join v1.div v2.div
    }

  let meet v1 v2 =
    { nor = V.meet v1.nor v2.nor;
      exn = V.meet v1.exn v2.exn;
      div = V.meet v1.div v2.div
    }

  let normal_return = { bot with nor = V.safe }

  let exn_escape = { bot with exn = V.safe }

  let diverges = { bot with div = V.safe }

  let safe = { nor = V.safe; exn = V.safe; div = V.safe }

  let top w = { nor = V.top w; exn = V.top w; div = V.top w }

  let relaxed w = { nor = V.safe; exn = V.top w; div = V.top w }

  let print ~witnesses ppf { nor; exn; div } =
    let pp = V.print ~witnesses in
    Format.fprintf ppf "{ nor=%a;@ exn=%a;@ div=%a }@," pp nor pp exn pp div

  let compare { nor = n1; exn = e1; div = d1 } { nor = n2; exn = e2; div = d2 }
      =
    let c = V.compare n1 n2 in
    if c <> 0
    then c
    else
      let c = V.compare e1 e2 in
      if c <> 0 then c else V.compare d1 d2
end
