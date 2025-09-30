external is_block : 'a or_null -> (int[@untagged])
  = "caml_is_block" "caml_is_block_untagged"
  [@@noalloc] [@@builtin]

external is_long : 'a or_null -> (int[@untagged])
  = "caml_is_tagged_immediate_or_null" "caml_is_tagged_immediate_or_null_untagged"
  [@@noalloc] [@@builtin]

let[@inline always] test_is_block x = if is_block x = 1 then 1 else 0

let[@inline always] test_is_long x = if is_long x = 1 then 1 else 0

let[@inline never] [@local never] wrap x =
  Sys.opaque_identity (This (Sys.opaque_identity x))

let name = "Test1"

let () =
  (* is_block *)
  let list = wrap ["a"; "b"; "c"] in
  Printf.printf "%s: is_block list = %d\n" name (test_is_block list);
  let pairs = wrap ((0, 1), (2, 3)) in
  Printf.printf "%s: is_block pairs = %d\n" name (test_is_block pairs);
  let float = wrap 7.5 in
  Printf.printf "%s: is_block float = %d\n" name (test_is_block float);
  let int = wrap 7 in
  Printf.printf "%s: is_block int = %d\n" name (test_is_block int);
  let unit = wrap () in
  Printf.printf "%s: is_block unit = %d\n" name (test_is_block unit);
  let n = Sys.opaque_identity Null in
  Printf.printf "%s: is_block null = %d\n" name (test_is_block n);
  (* is_long *)
  let list = wrap ["a"; "b"; "c"] in
  Printf.printf "%s: is_long list = %d\n" name (test_is_long list);
  let pairs = wrap ((0, 1), (2, 3)) in
  Printf.printf "%s: is_long pairs = %d\n" name (test_is_long pairs);
  let float = wrap 7.5 in
  Printf.printf "%s: is_long float = %d\n" name (test_is_long float);
  let int = wrap 7 in
  Printf.printf "%s: is_long int = %d\n" name (test_is_long int);
  let unit = wrap () in
  Printf.printf "%s: is_long unit = %d\n" name (test_is_long unit);
  let n = Sys.opaque_identity Null in
  Printf.printf "%s: is_long null = %d\n" name (test_is_long n);
  ()
