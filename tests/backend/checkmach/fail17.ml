[@@@zero_alloc on]
let f x ?(y = (x, x)) () = fst y + snd y
