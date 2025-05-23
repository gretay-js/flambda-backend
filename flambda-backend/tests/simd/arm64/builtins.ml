(* CR gyorsh: all instructions in this file can be renamed from "caml_neon" to
   "caml_simd" because they have the corresponding implementation on amd64. If
   we do it, [builtins.ml] in target specific folders will be identical, and we
   can move them up into the parent folder. If we keep support for both
   "caml_neon" and "caml_simd" in the compiler, we should add some tests for
   both versions. *)
module Float64 = struct
  type t = float

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external max_match_sse : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min_match_sse : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float64_sqrt"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64_round_near"
    [@@noalloc] [@@builtin]
end

module Float_cond_x86 = struct
  type t =
    | EQf
    | LTf
    | LEf
    | UNORDf
    | NEQf
    | NLTf
    | NLEf
    | ORDf

  let float_condition_of_int = function
    | 0 -> EQf
    | 1 -> LTf
    | 2 -> LEf
    | 3 -> UNORDf
    | 4 -> NEQf
    | 5 -> NLTf
    | 6 -> NLEf
    | 7 -> ORDf
    | n -> failwith (Printf.sprintf "Invalid float rounding immediate: %d" n)
end

module Int64x2 = struct
  type t = int64x2

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_srli"
    [@@noalloc] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_insert"
    [@@noalloc] [@@builtin]

  external bitwise_not : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_not"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_and : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float64x2 : t -> float64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_int32x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shifts with [count] in a register. See comment in [Int32x4]. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (neg count)

  let sra : t -> t -> t = fun arg count -> sshl arg (neg count)
end

module Int32x4 = struct
  type t = int32x4

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_srai"
    [@@noalloc] [@@builtin]

  external cvt_f32 : t -> float32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_float32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int32x2_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_f64 : t -> float64x2 =
   fun t -> t |> cvtsx_i64 |> Int64x2.cvt_float64x2

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int32x2_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_si16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_su16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_insert"
    [@@noalloc] [@@builtin]

  external bitwise_not : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_not"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_and : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shifts with [count] in a register.

     The function below match the semantics of amd64 shift builtins for
     instructions that operate on a register. The second argument [count] is an
     unsigned 128-bit integer (reinterpreting the declared type [t]). All lanes
     of the first argument [arg] are shifted by the same [count].

     The corresponding arm64 instructions expects a vector of signed [count]
     values, one per lane. If [count] is large then the bit-width of a lane, the
     shift is 0.

     There seems to be no arm64 instruction for right shift (logic or
     arithmetic) that take [count] in a register (not an immediate). The
     operation can be expressed using a negative count for the corresponding
     shift left instructions USHL and SSHL. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (neg (dup count))

  let sra : t -> t -> t = fun arg count -> sshl arg (neg (dup count))
end

module Float32x4 = struct
  type t = float32x4

  external cmeq : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmeq"
    [@@noalloc] [@@builtin]

  external cmge : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmge"
    [@@noalloc] [@@builtin]

  external cmgt : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmgt"
    [@@noalloc] [@@builtin]

  external cmle : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmle"
    [@@noalloc] [@@builtin]

  external cmlt : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmlt"
    [@@noalloc] [@@builtin]

  let is_nan t = Int32x4.bitwise_not (cmeq t t)

  let cmp n t1 t2 =
    match Float_cond_x86.float_condition_of_int n with
    | EQf -> cmeq t1 t2
    | LTf -> cmlt t1 t2
    | LEf -> cmle t1 t2
    | NEQf -> Int32x4.bitwise_not (cmeq t1 t2)
    | NLTf -> Int32x4.bitwise_not (cmlt t1 t2)
    | NLEf -> Int32x4.bitwise_not (cmle t1 t2)
    (* CR gyorsh: this is not efficient but gives us more testing coverage. *)
    | UNORDf -> Int32x4.bitwise_or (is_nan t1) (is_nan t2)
    | ORDf -> Int32x4.bitwise_not (Int32x4.bitwise_or (is_nan t1) (is_nan t2))

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rcp : t -> t = "caml_vec128_unreachable" "caml_neon_float32x4_rcp"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rsqrt : t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_rsqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float32x4_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_float32x4_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float64x2 : t -> float64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_float32x2_to_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_round_near"
    [@@noalloc] [@@builtin]
end

module Float64x2 = struct
  type t = float64x2

  (* Math *)

  external cmeq : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmeq"
    [@@noalloc] [@@builtin]

  external cmge : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmge"
    [@@noalloc] [@@builtin]

  external cmgt : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmgt"
    [@@noalloc] [@@builtin]

  external cmle : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmle"
    [@@noalloc] [@@builtin]

  external cmlt : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmlt"
    [@@noalloc] [@@builtin]

  let is_nan t = Int64x2.bitwise_not (cmeq t t)

  let cmp n t1 t2 =
    match Float_cond_x86.float_condition_of_int n with
    | EQf -> cmeq t1 t2
    | LTf -> cmlt t1 t2
    | LEf -> cmle t1 t2
    | NEQf -> Int64x2.bitwise_not (cmeq t1 t2)
    | NLTf -> Int64x2.bitwise_not (cmlt t1 t2)
    | NLEf -> Int64x2.bitwise_not (cmle t1 t2)
    (* CR gyorsh: this is not efficient but gives us more testing coverage. *)
    | UNORDf -> Int64x2.bitwise_or (is_nan t1) (is_nan t2)
    | ORDf -> Int64x2.bitwise_not (Int64x2.bitwise_or (is_nan t1) (is_nan t2))

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float64x2_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int64x2 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_float64x2_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float32x4 : t -> float32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_float64x2_to_float32x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* let cvt_int32x4 : t -> int32x4 = *)
  (*  fun t -> t |> cvt_float32x4 |> Float32x4.cvt_int32x4 *)

  let cvt_int32x4 : t -> int32x4 =
   fun t -> t |> cvt_int64x2 |> Int64x2.cvt_int32x2

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_round_near"
    [@@noalloc] [@@builtin]
end

module Int16x8 = struct
  type t = int16x8

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_si8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_su8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int16x8_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int16x8_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int16x8_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int16x8_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_hadd_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_avg_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minposu : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_minpos_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_high : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_high"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_high_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_high_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul_hadd_i32 : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_hadd_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_int16x8_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_insert"
    [@@noalloc] [@@builtin]

  external sll : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_sll"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_srai"
    [@@noalloc] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  let srl : t -> t -> t = fun arg count -> ushl arg (neg count)

  let sra : t -> t -> t = fun arg count -> sshl arg (neg count)
end

module Int8x16 = struct
  type t = int8x16

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int8x16_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int8x16_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int8x16_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int8x16_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int8x16_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int8x16_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_avg_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sadu : t -> t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_int8x16_sad_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external msadu :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_multi_sad_unsigned"
    [@@noalloc] [@@builtin]

  external mul_unsigned_hadd_saturating_i16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_int8x16_mul_unsigned_hadd_saturating_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_int8x16_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_insert"
    [@@noalloc] [@@builtin]
end

module SSE_Util = struct
  type t = int32x4

  external high_64_to_low_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_vec128_high_64_to_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_vec128_low_64_to_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_high_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_low_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external shuffle_32 :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shuffle_32"
    [@@noalloc] [@@builtin]

  (* CR gyorsh: [movemask_32] is not supported on arm64. This implementation
     uses [t < zero]. The result is in a completely different format:
     [movemask_32] creates a 4-bit mask with one bit of the mask set for each
     negative element of the input, whereas [cmpltz] sets all bits in the
     corresponding vector element of the result.

     The naive sequence below extracts the mask from from the result of
     [cmpltz]. *)
  (* CR-someday gyorsh: optimize the sequence for example see these blog posts:

     https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon

     : https://zeux.io/2022/09/02/vpexpandb-neon-z3/ *)
  let movemask_32 t =
    let mask = Int32x4.cmpltz t in
    let res = 0l in
    let i = 0 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 1 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 2 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 3 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    Int32.to_int res

  let movemask_64 t =
    let mask = Int64x2.cmpltz t in
    let res = 0L in
    let i = 0 in
    let lane_mask = Int64x2.extract i mask |> Int64.logand Int64.one in
    let res = Int64.logor res (Int64.shift_left lane_mask i) in
    let i = 1 in
    let lane_mask = Int64x2.extract i mask |> Int64.logand Int64.one in
    let res = Int64.logor res (Int64.shift_left lane_mask i) in
    Int64.to_int res
end

module SSE2_Util = struct
  external _and : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external andnot : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_andnot"
    [@@noalloc] [@@unboxed] [@@builtin]

  external _or : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external xor : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external movemask_8 : (int8x16[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_vec128_movemask_8"
    [@@noalloc] [@@builtin]

  external movemask_64 : (int64x2[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_vec128_movemask_64"
    [@@noalloc] [@@builtin]

  external shift_left_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shift_left_bytes"
    [@@noalloc] [@@builtin]

  external shift_right_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shift_right_bytes"
    [@@noalloc] [@@builtin]

  external shuffle_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shuffle_64"
    [@@noalloc] [@@builtin]

  external shuffle_high_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shuffle_high_16"
    [@@noalloc] [@@builtin]

  external shuffle_low_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shuffle_low_16"
    [@@noalloc] [@@builtin]

  external interleave_high_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_high_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_low_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_high_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_low_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSE3_Util = struct
  external dup_low_64 : int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_dup_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_odd_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_neon_vec128_dup_odd_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_even_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_neon_vec128_dup_even_32"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSSE3_Util = struct
  external shuffle_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_neon_vec128_shuffle_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external align_right_bytes :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_align_right_bytes"
    [@@noalloc] [@@builtin]
end

module SSE41_Util = struct
  external blend_16 :
    (int[@untagged]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) = "caml_vec128_unreachable" "caml_neon_vec128_blend_16"
    [@@noalloc] [@@builtin]

  external blend_32 :
    (int[@untagged]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) = "caml_vec128_unreachable" "caml_neon_vec128_blend_32"
    [@@noalloc] [@@builtin]

  external blend_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) = "caml_vec128_unreachable" "caml_neon_vec128_blend_64"
    [@@noalloc] [@@builtin]

  external blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_neon_vec128_blendv_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_neon_vec128_blendv_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_neon_vec128_blendv_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end

(* CR-someday gyorsh: neon doesn't seem to have dedicated instructions for
   comparing strings that correspond to [cmpestr*] and [cmpistr*] instructions
   from amd64. If needed, these can be implemented as sequences of intrinsics to
   match [SSE42_String] *)
