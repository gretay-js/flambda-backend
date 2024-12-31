[@@@ocaml.warnerror "+a-40-41-42"]

module Int32_u = struct
  type t = int32#

  external to_int32 : t -> (int32[@local_opt]) = "%box_int32" [@@warning "-187"]

  external of_int32 : (int32[@local_opt]) -> t = "%unbox_int32" [@@warning "-187"]

  let[@inline always] add x y = of_int32 (Int32.add (to_int32 x) (to_int32 y))
end

type t1 = { mutable d0 : int32# ; mutable d1: int32#; mutable d2: int32#; mutable d3: int32#  }

(* Currently, can't vectorize because of the sign extension. *)
let[@inline never] [@local never][@specialize never] add_unboxed_pairs_mutable_record (a : t1) (b: t1) (c : t1) : t1 =
  c.d0 <- Int32_u.add a.d0 b.d0;
  c.d1 <- Int32_u.add a.d1 b.d1;
  c.d2 <- Int32_u.add a.d2 b.d2;
  c.d3 <- Int32_u.add a.d3 b.d3;
  c

(*
  a:V/61 := R:I/0[%rax]
  b:V/62 := R:I/1[%rbx]
  c:V/63 := R:I/2[%rdi]
  I/64 := signed int32  mut[b:V/62]{test3.ml:16,27-31}
  I/65 := signed int32  mut[a:V/61]{test3.ml:16,22-26}
  I/66 := I/65
  I/66 := I/66 + I/64{test3.ml:16,10-31;test3.ml:10,41-78}
  I/67 := sextend32 I/66{test3.ml:16,10-31;test3.ml:10,41-78}
  signed int32[c:V/63] := I/67 (assign){test3.ml:16,2-31}
  I/68 := 1
  Psetmixedfield:I/69 := 1
  I/70 := signed int32  mut[b:V/62 + 8]{test3.ml:17,27-31}
  I/71 := signed int32  mut[a:V/61 + 8]{test3.ml:17,22-26}
  I/72 := I/71
  I/72 := I/72 + I/70{test3.ml:17,10-31;test3.ml:10,41-78}
  I/73 := sextend32 I/72{test3.ml:17,10-31;test3.ml:10,41-78}
  signed int32[c:V/63 + 8] := I/73 (assign){test3.ml:17,2-31}
  I/74 := 1
  Psetmixedfield:I/75 := 1
  I/76 := signed int32  mut[b:V/62 + 16]{test3.ml:18,27-31}
  I/77 := signed int32  mut[a:V/61 + 16]{test3.ml:18,22-26}
  I/78 := I/77
  I/78 := I/78 + I/76{test3.ml:18,10-31;test3.ml:10,41-78}
  I/79 := sextend32 I/78{test3.ml:18,10-31;test3.ml:10,41-78}
  signed int32[c:V/63 + 16] := I/79 (assign){test3.ml:18,2-31}
  I/80 := 1
  Psetmixedfield:I/81 := 1
  I/82 := signed int32  mut[b:V/62 + 24]{test3.ml:19,27-31}
  I/83 := signed int32  mut[a:V/61 + 24]{test3.ml:19,22-26}
  I/84 := I/83
  I/84 := I/84 + I/82{test3.ml:19,10-31;test3.ml:10,41-78}
  I/85 := sextend32 I/84{test3.ml:19,10-31;test3.ml:10,41-78}
  signed int32[c:V/63 + 24] := I/85 (assign){test3.ml:19,2-31}
  I/86 := 1
  Psetmixedfield:I/87 := 1
  R:I/0[%rax] := c:V/63
  return R:I/0[%rax]

*)

let print_t1 ppf (t1 : t1) =
  Format.fprintf ppf "{ d0 = %ld ; d1 = %ld; d2 = %ld ; d3 = %ld }"
    (Int32_u.to_int32 t1.d0)
    (Int32_u.to_int32 t1.d1)
    (Int32_u.to_int32 t1.d2)
    (Int32_u.to_int32 t1.d3)

let () =
  let a = { d0 = #8l; d1 = #96l; d2 = -#10l; d3 = #0l } in
  let b = { d0 = #80l; d1 = #14l; d2 = -#30l; d3 = -#100l } in
  let c = { d0 = #8l; d1 = #96l; d2 = #0l; d3 = #0l } in
  Format.printf "add_unboxed_pairs_mutable_record %a\n" print_t1
    (add_unboxed_pairs_mutable_record a b c);
  ()
