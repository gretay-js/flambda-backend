[@@@zero_alloc on]

(* For duplicate attributes we don't get a warning, but the first one takes effect, so
   [fails] is not assumed to pass the check, and [call] fails the check. *)
let[@inline never][@zero_alloc off][@zero_alloc assume strict] fails x = (x,x)

let[@zero_alloc strict] call x = fails (x+1)
