let eqi lv hv l h =
  if l <> lv then Printf.printf "%016x <> %016x\n" lv l;
  if h <> hv then Printf.printf "%016x <> %016x\n" hv h;
  if l <> lv || h <> hv then !failmsg ()

module Float32 = Float32_reference

module Float32x4 = struct
  include Builtins.Float32x4
  include Sse_other_builtins.Float32x4

  let () =
    Float32.check_floats (fun f0 f1 ->
        (failmsg
           := fun () ->
                Printf.printf "dpf32 %f %f\n%!" (Int32.float_of_bits f0)
                  (Int32.float_of_bits f1));
        let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
        let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = dp 0b1111_0001 fv0 fv1 in
        let expect =
          Float32.to_float32x4
            (Float32.add
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0)))
            0l 0l 0l
        in
        (* When both are NaN, AMD returns the first argument and Intel returns
           the second argument. Hence we do not test this case. *)
        if f0 |> Int32.float_of_bits |> Float.is_nan
           && f1 |> Int32.float_of_bits |> Float.is_nan
        then ()
        else eq_float32x4 ~result ~expect)

  let () =
    Test_helpers.run_if_not_under_rosetta2 ~f:(fun () ->
        Float32.check_floats (fun f0 f1 ->
            (failmsg
               := fun () ->
                    Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                      (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f0 f1 f1 in
            let fv1 = Float32.to_float32x4 f1 f1 f0 f0 in
            let result = addsub fv0 fv1 in
            let expect =
              Float32.to_float32x4 (Float32.sub f0 f1) (Float32.add f0 f1)
                (Float32.sub f1 f0) (Float32.add f1 f0)
            in
            eq_float32x4 ~result ~expect);
        Float32.check_floats (fun f0 f1 ->
            (failmsg
               := fun () ->
                    Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                      (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
            let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
            let result = hsub fv0 fv1 in
            let expect =
              Float32.to_float32x4 (Float32.sub f0 f1) (Float32.sub f0 f1)
                (Float32.sub f1 f0) (Float32.sub f1 f0)
            in
            eq_float32x4 ~result ~expect))
end

module Int64 = struct
  include Sse_other_builtins.Int64

  let eq' x y = if x <> y then Printf.printf "%016Lx <> %016Lx\n" x y

  let () =
    Test_helpers.run_if_not_under_rosetta2 ~f:(fun () ->
        eq' (bit_deposit 3L 4L) 0x4L;
        eq' (bit_deposit 235L 522L) 0xAL;
        eq' (bit_extract 3L 4L) 0x0L;
        eq' (bit_extract 235L 522L) 0x3L)
end

module Int64x2 = struct
  type t = int64x2

  external int64x2_of_int64s : int64 -> int64 -> int64x2
    = "caml_vec128_unreachable" "vec128_of_int64s"
    [@@noalloc] [@@unboxed]

  external int64x2_low_int64 : int64x2 -> int64
    = "caml_vec128_unreachable" "vec128_low_int64"
    [@@noalloc] [@@unboxed]

  external int64x2_high_int64 : int64x2 -> int64
    = "caml_vec128_unreachable" "vec128_high_int64"
    [@@noalloc] [@@unboxed]

  external clmul :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_clmul_int64x2"
    [@@noalloc] [@@builtin]

  let eq lv hv l h =
    if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
    if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h

  let () =
    let v0 = int64x2_of_int64s 3L 4L in
    let v1 = int64x2_of_int64s 5L 12L in
    let c0 = clmul 0b0000_0000 v0 v1 in
    let c1 = clmul 0b0000_0001 v0 v1 in
    let c2 = clmul 0b0001_0000 v0 v1 in
    let c3 = clmul 0b0001_0001 v0 v1 in
    eq (int64x2_low_int64 c0) (int64x2_high_int64 c0) 15L 0L;
    eq (int64x2_low_int64 c1) (int64x2_high_int64 c1) 20L 0L;
    eq (int64x2_low_int64 c2) (int64x2_high_int64 c2) 20L 0L;
    eq (int64x2_low_int64 c3) (int64x2_high_int64 c3) 48L 0L
end

module Int32s = struct
  external int32x4_of_int64s : int64 -> int64 -> int32x4
    = "caml_vec128_unreachable" "vec128_of_int64s"
    [@@noalloc] [@@unboxed]

  let of_int32s a b c d =
    let a = Int64.of_int32 a |> Int64.logand 0xffffffffL in
    let b = Int64.of_int32 b |> Int64.logand 0xffffffffL in
    let c = Int64.of_int32 c |> Int64.logand 0xffffffffL in
    let d = Int64.of_int32 d |> Int64.logand 0xffffffffL in
    int32x4_of_int64s
      Int64.(logor (shift_left b 32) a)
      Int64.(logor (shift_left d 32) c)
end

module SSE_Util = struct
  let () =
    let v = Int32s.of_int32s 0xffffffffl 0x80000000l 0x7fffffffl 0x0l in
    let i = Builtins.SSE_Utils.movemask_32 v in
    eqi i 0 0b0011 0
end
