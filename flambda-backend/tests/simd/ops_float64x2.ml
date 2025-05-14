open Utils
include Builtins.Float64x2
(* Creation / Destruction *)

external low_of : float -> t
  = "caml_vec128_unreachable" "caml_float64x2_low_of_float"
  [@@noalloc] [@@unboxed] [@@builtin]

external low_to : t -> float
  = "caml_vec128_unreachable" "caml_float64x2_low_to_float"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  let v1 = low_of 1. in
  let v2 = low_of 2. in
  let i1 = float64x2_low_int64 v1 in
  let i2 = float64x2_low_int64 v2 in
  eq i1 i2 0x3ff0000000000000L 0x4000000000000000L;
  let f1 = low_to v1 in
  let f2 = low_to v2 in
  eqf f1 f2 1. 2.

let to_float64x2 f0 f1 =
  let v0, v1 = Int64.bits_of_float f0, Int64.bits_of_float f1 in
  float64x2_of_int64s v0 v1

(* Math *)

let check_cmp msg scalar vector f0 f1 =
  (failmsg := fun () -> Printf.printf "check_cmp64 %s: %f | %f\n%!" msg f0 f1);
  let r0, m0 = if scalar f0 f1 then 0xffffffffffffffffL, 1 else 0L, 0 in
  let r1, m1 = if scalar f1 f0 then 0xffffffffffffffffL, 1 else 0L, 0 in
  let expect = int64x2_of_int64s r0 r1 in
  let expect_mask = m0 lor (m1 lsl 1) in
  let v1 = to_float64x2 f0 f1 in
  let v2 = to_float64x2 f1 f0 in
  let result = vector v1 v2 in
  let mask = Builtins.SSE_Util.movemask_64 result in
  eqi mask mask expect_mask (Builtins.SSE_Util.movemask_64 expect);
  eq (int64x2_low_int64 result)
    (int64x2_high_int64 result)
    (int64x2_low_int64 expect)
    (int64x2_high_int64 expect)

let () =
  let remove_nan p l r = p l r && not (Float.is_nan l || Float.is_nan r) in
  let add_nan p l r = p l r || Float.is_nan l || Float.is_nan r in
  Float64.check_floats
    (check_cmp "0" (remove_nan Float.equal) (fun l r -> cmp 0 l r));
  Float64.check_floats
    (check_cmp "1"
       (remove_nan (fun l r -> Float.compare l r = -1))
       (fun l r -> cmp 1 l r));
  Float64.check_floats
    (check_cmp "2"
       (remove_nan (fun l r -> Float.compare l r <= 0))
       (fun l r -> cmp 2 l r));
  Float64.check_floats
    (check_cmp "3"
       (fun l r -> Float.is_nan l || Float.is_nan r)
       (fun l r -> cmp 3 l r));
  Float64.check_floats
    (check_cmp "4"
       (fun l r ->
         (not (Float.equal l r)) || (Float.is_nan l && Float.is_nan r))
       (fun l r -> cmp 4 l r));
  Float64.check_floats
    (check_cmp "5"
       (add_nan (fun l r -> Float.compare l r >= 0))
       (fun l r -> cmp 5 l r));
  Float64.check_floats
    (check_cmp "6"
       (add_nan (fun l r -> Float.compare l r = 1))
       (fun l r -> cmp 6 l r));
  Float64.check_floats
    (check_cmp "7"
       (fun l r -> (not (Float.is_nan l)) && not (Float.is_nan r))
       (fun l r -> cmp 7 l r))

let check_binop scalar vector f0 f1 =
  (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
  let r0 = scalar f0 f1 in
  let r1 = scalar f1 f0 in
  let expect = to_float64x2 r0 r1 in
  let v1 = to_float64x2 f0 f1 in
  let v2 = to_float64x2 f1 f0 in
  let result = vector v1 v2 in
  eq_float64x2 ~result ~expect

let () =
  let preserve_nan p l r =
    if Float.is_nan r then r else if Float.is_nan l then r else p l r
  in
  let preserve_zero p l r =
    if (l = 0.0 || l = -0.0) && (r = 0.0 || r = -0.0) then r else p l r
  in
  Float64.check_floats (check_binop Float.add add);
  Float64.check_floats (check_binop Float.sub sub);
  Float64.check_floats (check_binop Float.mul mul);
  Float64.check_floats (check_binop Float.div div);
  Float64.check_floats
    (check_binop (Float.max |> preserve_nan |> preserve_zero) max);
  Float64.check_floats
    (check_binop (Float.min |> preserve_nan |> preserve_zero) min);
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "sqrt %f | %f\n%!" f0 f1);
      let fv = to_float64x2 f0 f1 in
      let res = sqrt fv in
      eq (float64x2_low_int64 res) (float64x2_high_int64 res)
        (Int64.bits_of_float (Float.sqrt f0))
        (Int64.bits_of_float (Float.sqrt f1)))

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "cvti32 %f | %f\n%!" f0 f1);
      let i0 =
        Int32.of_float (Float.round f0)
        |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Int32.of_float (Float.round f1)
        |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let ii = Int64.(logor (shift_left i1 32) i0) in
      let iv = int32x4_of_int64s ii 0L in
      let fv = to_float64x2 f0 f1 in
      let res = cvt_int32x4 fv in
      eq (int32x4_low_int64 res) (int32x4_high_int64 res) (int32x4_low_int64 iv)
        (int32x4_high_int64 iv));
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "cvtf32 %f %f\n%!" f0 f1);
      let i0 =
        Int32.bits_of_float f0 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let i1 =
        Int32.bits_of_float f1 |> Int64.of_int32 |> Int64.logand 0xffffffffL
      in
      let ii = Int64.(logor (shift_left i1 32) i0) in
      let iv = float32x4_of_int64s ii 0L in
      let fv = to_float64x2 f0 f1 in
      let res = cvt_float32x4 fv in
      eq_float32x4 ~result:res ~expect:iv)

let () =
  Test_helpers.run_if_not_under_rosetta2 ~f:(fun () ->
      Float64.check_floats (fun f0 f1 ->
          (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
          let fv0 = to_float64x2 f0 f0 in
          let fv1 = to_float64x2 f1 f1 in
          let result = addsub fv0 fv1 in
          let expect = to_float64x2 (f0 -. f1) (f0 +. f1) in
          eq_float64x2 ~result ~expect);
      Float64.check_floats (fun f0 f1 ->
          (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
          let fv0 = to_float64x2 f0 f0 in
          let fv1 = to_float64x2 f1 f1 in
          let result = hadd fv0 fv1 in
          let expect = to_float64x2 (f0 +. f0) (f1 +. f1) in
          eq_float64x2 ~result ~expect);
      Float64.check_floats (fun f0 f1 ->
          (failmsg := fun () -> Printf.printf "%f | %f\n%!" f0 f1);
          let fv0 = to_float64x2 f0 f1 in
          let fv1 = to_float64x2 f1 f0 in
          let result = hsub fv0 fv1 in
          let expect = to_float64x2 (f0 -. f1) (f1 -. f0) in
          eq_float64x2 ~result ~expect))

let () =
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "%f dp %f\n%!" f0 f1);
      let fv0 = to_float64x2 f0 f1 in
      let fv1 = to_float64x2 f1 f0 in
      let result = dp 0b0011_0001 fv0 fv1 in
      let expect = to_float64x2 ((f0 *. f1) +. (f1 *. f0)) 0.0 in
      eq_float64x2 ~result ~expect);
  Float64.check_floats (fun f0 f1 ->
      (failmsg := fun () -> Printf.printf "roundf64 %f %f\n%!" f0 f1);
      let fv = to_float64x2 f0 f1 in
      let result = round 0x8 fv in
      let expect = to_float64x2 (Float64.c_round f0) (Float64.c_round f1) in
      eq_float64x2 ~result ~expect)
