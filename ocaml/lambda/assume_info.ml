type t =
  | No_assume
  | Assume of { strict : bool; never_returns_normally : bool }
(*

   The meaning of keywords [strict] and [never_returns_normally]
   is defined in terms of abstract values as follows:

   relaxed (default):         nor = Safe and exn = Top  and div = Top
   strict:                    nor = Safe and exn = Safe and div = Safe
   never_returns_normally:    nor = Bot  and exn = Top  and div = Top

   where [nor] means normal return of the call, [exn] means return via an exception,
   [div] means diverging (non-terminating) executions,
   and the meaning and order of elements is:

   Top    may allocate
   Safe   does not allocate on any execution paths
   Bot    unreachable

   Using more than one keyword means intersection (i.e., meet of the  elements,
   pointwise lifted to tuples), so we get the following:

   [@zero_alloc assume]                               nor = Safe and exn = Top  and div = Top
   [@zero_alloc assume strict]                        nor = Safe and exn = Safe and div = Safe
   [@zero_alloc assume strict never_returns_normally] nor = Bot  and exn = Safe and div = Safe
   [@zero_alloc assume never_returns_normally]        nor = Bot  and exn = Top  and div = Top

   See [Value] and [Annotation] in [backend/checkmach.ml].
*)
(* CR gyorsh: should we move [Value] and [Annotation] here or maybe "utils" and use them
   directly, instead of the weird compare function that abstracts them? Perhaps we
   should translate "strict" and "never_returns_normally" directly into (nor,exn,div)
*)

let[@inline always] rank = function
  | No_assume -> 0
  | Assume { strict = false; never_returns_normally = false } -> 1
  | Assume { strict = true; never_returns_normally = false } -> 2
  | Assume { strict = false; never_returns_normally = true } -> 3
  | Assume { strict = true; never_returns_normally = true } -> 4

let compare t1 t2 = Int.compare (rank t1) (rank t2)
let equal t1 t2 = Int.equal (rank t1) (rank t2)

let to_string = function
  | No_assume -> ""
  | Assume { strict; never_returns_normally } ->
      Printf.sprintf "(assume zero_alloc%s%s)"
        ( if strict then
          "_strict"
        else
          ""
        )
        ( if never_returns_normally then
          "_never_returns_normally"
        else
          ""
        )

let join t1 t2 =
  (* max of rank *)
  if Int.compare (rank t1) (rank t2) < 0 then
    t2
  else
    t1

let meet t1 t2 =
  (* min of rank *)
  if Int.compare (rank t1) (rank t2) < 0 then
    t1
  else
    t2
