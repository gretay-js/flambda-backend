
let[@inline always] test46 x = if x > 0 then failwith (Printf.sprintf "%d" x) else (x,x)

let[@zero_alloc strict] test48 x =
  (test46[@zero_alloc assume never_returns_normally]) x

(* This example shows that  never_returns_normally only works on calls, not on allocation. *)
let[@zero_alloc] test49 x =
  try let y = (test46[@zero_alloc assume never_returns_normally]) x in [y;(x,x+1)]
  with _ -> failwith (Printf.sprintf "%d" x)
