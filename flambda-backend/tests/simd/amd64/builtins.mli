module Float64 : sig
  type t = float

  val max : t -> t -> t

  val min : t -> t -> t


  val sqrt : t -> t

  val round : (int) -> (t) -> (t)

end

module Float32x4 : sig
  type t = float32x4

  val cmp :
    (int) -> (t) -> (t) -> (int32x4)

  val movemask_32 : (int32x4[@unboxed]) -> (int[@untagged])


  val add : t -> t -> t

  val sub : t -> t -> t


  val mul : t -> t -> t



  val div : t -> t -> t



  val max : t -> t -> t



  val min : t -> t -> t



  val rcp : t -> t


  val rsqrt : t -> t


  val sqrt : t -> t


  val cvt_int32x4 : t -> int32x4



  val cvt_float64x2 : t -> float64x2



  val addsub : t -> t -> t



  val hadd : t -> t -> t



  val hsub : t -> t -> t



  val dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])

  val round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])

end

module Float64x2 : sig
  type t = float64x2



  val cmp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_float64x2_cmp"


  val movemask_64 : (int64x2[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"


  val add : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_add"


  val sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_sub"


  val mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_mul"


  val div : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_div"


  val max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_max"


  val min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_float64x2_min"


  val sqrt : t -> t = "caml_vec128_unreachable" "caml_sse2_float64x2_sqrt"


  val cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_int32x4"


  val cvt_float32x4 : t -> float32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_float64x2_float32x4"


  val addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_addsub"


  val hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hadd"


  val hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hsub"


  val dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_dp"


  val round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_round"

end

module Int64x2 : sig
  type t = int64x2

  val add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_add"


  val sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sub"


  val cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int64x2_cmpeq"


  val cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse42_int64x2_cmpgt"


  val sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_sll"


  val srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int64x2_srl"


  val slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int64x2_slli"


  val srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int64x2_srli"


  val extract : (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int64x2_extract"


  val insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int64x2_insert"

end

module Int32x4 : sig
  type t = int32x4

  val add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_add"


  val sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sub"


  val cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpeq"


  val cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpgt"


  val sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sll"


  val srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_srl"


  val sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int32x4_sra"


  val slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_slli"


  val srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_srli"


  val srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int32x4_srai"


  val cvt_f64 : t -> float64x2
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float64x2"


  val cvt_f32 : t -> float32x4
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_float32x4"


  val abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int32x4_abs"


  val hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hadd"


  val hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hsub"


  val mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_mulsign"


  val max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_max"


  val max_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_max_unsigned"


  val min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_min"


  val min_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_min_unsigned"


  val cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int32x4_int64x2"


  val cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int32x4_int64x2"


  val cvt_si16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating"


  val cvt_su16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned"


  val mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int32x4_mul_low"


  val extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int32x4_extract"


  val insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int32x4_insert"

end

module Int16x8 : sig
  type t = int16x8

  val add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_add"


  val add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating"


  val add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_add_saturating_unsigned"


  val sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sub"


  val sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating"


  val sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_sub_saturating_unsigned"


  val max : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_max"


  val min : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_min"


  val maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_max_unsigned"


  val minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_min_unsigned"


  val cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpeq"


  val cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_cmpgt"


  val cvt_si8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating"


  val cvt_su8 : t -> t -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned"


  val cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int32x4"


  val cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int16x8_int64x2"


  val cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int32x4"


  val cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int16x8_int64x2"


  val abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int16x8_abs"


  val hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd"


  val hadd_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hadd_saturating"


  val hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub"


  val hsub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub_saturating"


  val mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_mulsign"


  val avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_avg_unsigned"


  val minposu : t -> t
    = "caml_vec128_unreachable" "caml_sse41_int16x8_minpos_unsigned"


  val mul_high : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_high"


  val mul_high_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_high_unsigned"


  val mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_low"


  val mul_hadd_i32 : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_sse2_int16x8_mul_hadd_int32x4"


  val extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse41_int16x8_extract"


  val insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int16x8_insert"


  val sll : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sll"


  val srl : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_srl"


  val sra : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int16x8_sra"


  val slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_slli"


  val srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_srli"


  val srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_int16x8_srai"

end

module Int8x16 : sig
  type t = int8x16

  val add : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_add"


  val add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating"


  val add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_add_saturating_unsigned"


  val sub : t -> t -> t = "caml_vec128_unreachable" "caml_sse2_int8x16_sub"


  val sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating"


  val sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sub_saturating_unsigned"


  val max : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int8x16_max"


  val min : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse41_int8x16_min"


  val maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_max_unsigned"


  val minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_min_unsigned"


  val cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpeq"


  val cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_cmpgt"


  val cvtsx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int16x8"


  val cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int32x4"


  val cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtsx_int8x16_int64x2"


  val cvtzx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int16x8"


  val cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int32x4"


  val cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_cvtzx_int8x16_int64x2"


  val abs : t -> t = "caml_vec128_unreachable" "caml_ssse3_int8x16_abs"


  val mulsign : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int8x16_mulsign"


  val avgu : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse2_int8x16_avg_unsigned"


  val sadu : t -> t -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_int8x16_sad_unsigned"


  val msadu :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_multi_sad_unsigned"


  val mul_unsigned_hadd_saturating_i16 : t -> t -> int16x8
    = "caml_vec128_unreachable" "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8"


  val extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_extract"


  val insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_int8x16_insert"

end

module SSE_Util : sig
  type t = int32x4

  val high_64_to_low_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_high_64_to_low_64"


  val low_64_to_high_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_low_64_to_high_64"


  val interleave_high_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_interleave_high_32"


  val interleave_low_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse_vec128_interleave_low_32"


  val shuffle_32 :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse_vec128_shuffle_32"


  val movemask_32 : (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse_vec128_movemask_32"

end

module SSE2_Util : sig
  val _and : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_and"


  val andnot : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_andnot"


  val _or : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_or"


  val xor : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_xor"


  val movemask_8 : (int8x16[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_8"


  val movemask_64 : (int64x2[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_sse2_vec128_movemask_64"


  val shift_left_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shift_left_bytes"


  val shift_right_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shift_right_bytes"


  val shuffle_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_64"


  val shuffle_high_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_high_16"


  val shuffle_low_16 :
    (int[@untagged]) -> (int16x8[@unboxed]) -> (int16x8[@unboxed])
    = "caml_vec128_unreachable" "caml_sse2_vec128_shuffle_low_16"


  val interleave_high_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_8"


  val interleave_low_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_8"


  val interleave_high_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_16"


  val interleave_low_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_16"


  val interleave_high_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_high_64"


  val interleave_low_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse2_vec128_interleave_low_64"

end

module SSE3_Util : sig
  val dup_low_64 : int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_low_64"


  val dup_odd_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_odd_32"


  val dup_even_32 : int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse3_vec128_dup_even_32"

end

module SSSE3_Util : sig
  val shuffle_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_ssse3_vec128_shuffle_8"


  val align_right_bytes :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_ssse3_vec128_align_right_bytes"

end

module SSE41_Util : sig
  val blend_16 :
    (int[@untagged]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_16"


  val blend_32 :
    (int[@untagged]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_32"


  val blend_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_64"


  val blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_8"

  val blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_32"

  val blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_64"

end
