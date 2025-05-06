module Float32x4 = struct
  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float32x4_dp"
    [@@noalloc] [@@builtin]

  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_hsub"
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
