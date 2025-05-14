module Float32x4 = struct
  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float32x4_dp"
    [@@noalloc] [@@builtin]
end

module Float64x2 = struct
  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_dp"
    [@@noalloc] [@@builtin]
end

module Int32x4 = struct
  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module Int64 = struct
  type t = int64

  external bit_deposit : t -> t -> t
    = "caml_vec128_unreachable" "caml_bmi2_int64_deposit_bits"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bit_extract : t -> t -> t
    = "caml_vec128_unreachable" "caml_bmi2_int64_extract_bits"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module Int16x8 = struct
  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]
end
