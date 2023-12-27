let[@zero_alloc assume][@inline never][@specialise never][@local never] bar x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo x = bar x


let[@zero_alloc assume][@inline always] bar' x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo' x = bar' x

let[@inline always] test46 x = if x > 0 then failwith (Printf.sprintf "%d" x) else (x,x)

let[@zero_alloc strict] test48 x =
  (test46[@zero_alloc assume never_returns_normally]) x

(* Perhaps confusingly, never_returns_normally works on allocations not only
   on calls. This is needed for analysis to give the same results regardess
   of inlining of expressions annotated with "assume".  *)
let[@zero_alloc] test49 x =
  try let y = (test46[@zero_alloc assume never_returns_normally]) x in [y;(x,x+1)]
  with _ -> failwith (Printf.sprintf "%d" x)

