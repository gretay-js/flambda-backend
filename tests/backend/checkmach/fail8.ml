(* fail because conservate about the call to forever3 does not know that
   the call never returns and so the allocation is not reachable. *)
let[@zero_alloc strict][@inline never] rec test30 () =
  forever3 ();
  (2, 3)
and[@zero_alloc strict][@inline never] forever3 () = while true do () done
