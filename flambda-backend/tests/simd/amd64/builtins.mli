module Float64 : sig
  type t = float

  val max : t -> t -> t

  val min : t -> t -> t

  val sqrt : t -> t

  (* val round : int -> t -> t *)
  external round : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "" "caml_sse41_float64_round"
    [@@noalloc] [@@builtin]
end

module Float32x4 : sig
  type t = float32x4

  (* val cmp : int -> t -> t -> int32x4 *)
  external cmp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_sse_float32x4_cmp"
    [@@noalloc] [@@builtin]

  val movemask_32 : int32x4 -> int

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

  val dp : int -> t -> t -> t

  val round : int -> t -> t
end

module Float64x2 : sig
  type t = float64x2

  val cmp : int -> t -> t -> int64x2

  val movemask_64 : int64x2 -> int

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val sqrt : t -> t

  val cvt_int32x4 : t -> int32x4

  val cvt_float32x4 : t -> float32x4

  val addsub : t -> t -> t

  val hadd : t -> t -> t

  val hsub : t -> t -> t

  val dp : int -> t -> t -> t

  val round : int -> t -> t
end

module Int64x2 : sig
  type t = int64x2

  val add : t -> t -> t

  val sub : t -> t -> t

  val cmpeq : t -> t -> t

  val cmpgt : t -> t -> t

  val sll : t -> t -> t

  val srl : t -> t -> t

  val slli : int -> t -> t

  val srli : int -> t -> t

  val extract : int -> t -> int64

  val insert : int -> t -> int64 -> t
end

module Int32x4 : sig
  type t = int32x4

  val add : t -> t -> t

  val sub : t -> t -> t

  val cmpeq : t -> t -> t

  val cmpgt : t -> t -> t

  val sll : t -> t -> t

  val srl : t -> t -> t

  val sra : t -> t -> t

  val slli : int -> t -> t

  val srli : int -> t -> t

  val srai : int -> t -> t

  val cvt_f64 : t -> float64x2

  val cvt_f32 : t -> float32x4

  val abs : t -> t

  val hadd : t -> t -> t

  val hsub : t -> t -> t

  val mulsign : t -> t -> t

  val max : t -> t -> t

  val max_unsigned : t -> t -> t

  val min : t -> t -> t

  val min_unsigned : t -> t -> t

  val cvtsx_i64 : t -> int64x2

  val cvtzx_i64 : t -> int64x2

  val cvt_si16 : t -> t -> int16x8

  val cvt_su16 : t -> t -> int16x8

  val mul_low : t -> t -> t

  val extract : int -> t -> int32

  val insert : int -> t -> int32 -> t
end

module Int16x8 : sig
  type t = int16x8

  val add : t -> t -> t

  val add_saturating : t -> t -> t

  val add_saturating_unsigned : t -> t -> t

  val sub : t -> t -> t

  val sub_saturating : t -> t -> t

  val sub_saturating_unsigned : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val maxu : t -> t -> t

  val minu : t -> t -> t

  val cmpeq : t -> t -> t

  val cmpgt : t -> t -> t

  val cvt_si8 : t -> t -> int8x16

  val cvt_su8 : t -> t -> int8x16

  val cvtsx_i32 : t -> int32x4

  val cvtsx_i64 : t -> int64x2

  val cvtzx_i32 : t -> int32x4

  val cvtzx_i64 : t -> int64x2

  val abs : t -> t

  val hadd : t -> t -> t

  val hadd_saturating : t -> t -> t

  val hsub : t -> t -> t

  val hsub_saturating : t -> t -> t

  val mulsign : t -> t -> t

  val avgu : t -> t -> t

  val minposu : t -> t

  val mul_high : t -> t -> t

  val mul_high_unsigned : t -> t -> t

  val mul_low : t -> t -> t

  val mul_hadd_i32 : t -> t -> int32x4

  val extract : int -> t -> int

  val insert : int -> t -> int -> t

  val sll : t -> t -> t

  val srl : t -> t -> t

  val sra : t -> t -> t

  val slli : int -> t -> t

  val srli : int -> t -> t

  val srai : int -> t -> t
end

module Int8x16 : sig
  type t = int8x16

  val add : t -> t -> t

  val add_saturating : t -> t -> t

  val add_saturating_unsigned : t -> t -> t

  val sub : t -> t -> t

  val sub_saturating : t -> t -> t

  val sub_saturating_unsigned : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val maxu : t -> t -> t

  val minu : t -> t -> t

  val cmpeq : t -> t -> t

  val cmpgt : t -> t -> t

  val cvtsx_i16 : t -> int16x8

  val cvtsx_i32 : t -> int32x4

  val cvtsx_i64 : t -> int64x2

  val cvtzx_i16 : t -> int16x8

  val cvtzx_i32 : t -> int32x4

  val cvtzx_i64 : t -> int64x2

  val abs : t -> t

  val mulsign : t -> t -> t

  val avgu : t -> t -> t

  val sadu : t -> t -> int64x2

  val msadu : int -> t -> t -> int16x8

  val mul_unsigned_hadd_saturating_i16 : t -> t -> int16x8

  val extract : int -> t -> int

  val insert : int -> t -> int -> t
end

module SSE_Util : sig
  type t = int32x4

  val high_64_to_low_64 : t -> t -> t

  val low_64_to_high_64 : t -> t -> t

  val interleave_high_32 : t -> t -> t

  val interleave_low_32 : t -> t -> t

  val shuffle_32 : int -> t -> t -> t

  val movemask_32 : t -> int
end

module SSE2_Util : sig
  val _and : int64x2 -> int64x2 -> int64x2

  val andnot : int64x2 -> int64x2 -> int64x2

  val _or : int64x2 -> int64x2 -> int64x2

  val xor : int64x2 -> int64x2 -> int64x2

  val movemask_8 : int8x16 -> int

  val movemask_64 : int64x2 -> int

  val shift_left_bytes : int -> int8x16 -> int8x16

  val shift_right_bytes : int -> int8x16 -> int8x16

  val shuffle_64 : int -> int64x2 -> int64x2 -> int64x2

  val shuffle_high_16 : int -> int16x8 -> int16x8

  val shuffle_low_16 : int -> int16x8 -> int16x8

  val interleave_high_8 : int8x16 -> int8x16 -> int8x16

  val interleave_low_8 : int8x16 -> int8x16 -> int8x16

  val interleave_high_16 : int16x8 -> int16x8 -> int16x8

  val interleave_low_16 : int16x8 -> int16x8 -> int16x8

  val interleave_high_64 : int64x2 -> int64x2 -> int64x2

  val interleave_low_64 : int64x2 -> int64x2 -> int64x2
end

module SSE3_Util : sig
  val dup_low_64 : int64x2 -> int64x2

  val dup_odd_32 : int32x4 -> int32x4

  val dup_even_32 : int32x4 -> int32x4
end

module SSSE3_Util : sig
  val shuffle_8 : int8x16 -> int8x16 -> int8x16

  val align_right_bytes : int -> int8x16 -> int8x16 -> int8x16
end

module SSE41_Util : sig
  val blend_16 : int -> int16x8 -> int16x8 -> int16x8

  val blend_32 : int -> int32x4 -> int32x4 -> int32x4

  val blend_64 : int -> int64x2 -> int64x2 -> int64x2

  val blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16

  val blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4

  val blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2
end
