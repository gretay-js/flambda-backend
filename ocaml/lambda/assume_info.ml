type t =
  | No_assume
  | Assume of { strict : bool; never_returns_normally : bool }

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
      (if strict then "_strict" else "")
      (if never_returns_normally then "_never_returns_normally" else "")

let join t1 t2 =
  (* max of rank *)
  if Int.compare (rank t1) (rank t2) < 0 then
    t2
  else
    t1

let meet t1 t2 =
  (* mix of rank *)
  if Int.compare (rank t1) (rank t2) < 0 then
    t1
  else
    t2



