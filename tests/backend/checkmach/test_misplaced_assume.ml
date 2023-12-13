[@@@warnings "+53"]

let[@inline never] bar x = (x,x)

let[@zero_alloc] foo x =
  ((bar x)[@zero_alloc assume])

let[@zero_alloc] f =
  let x = 42 in
  fun z -> z + x
