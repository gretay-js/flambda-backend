(* Test for 128-bit vector constants using primitive types *)

(* Vector types int64x2 and float64x2 are predefined in OCaml *)

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64"
[@@noalloc] [@@unboxed]

external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64"
[@@noalloc] [@@unboxed]

external float64x2_of_int64s : int64 -> int64 -> float64x2 = "" "vec128_of_int64s"
[@@noalloc] [@@unboxed]

external float64x2_low_int64 : float64x2 -> int64 = "" "vec128_low_int64"
[@@noalloc] [@@unboxed]

external float64x2_high_int64 : float64x2 -> int64 = "" "vec128_high_int64"
[@@noalloc] [@@unboxed]

(* Create some vector constants *)
let int_vec_zero = int64x2_of_int64s 0L 0L
let int_vec_one = int64x2_of_int64s 1L 1L
let int_vec_ascending = int64x2_of_int64s 1L 2L
let int_vec_max = int64x2_of_int64s Int64.max_int Int64.max_int
let int_vec_min = int64x2_of_int64s Int64.min_int Int64.min_int
let int_vec_mixed = int64x2_of_int64s 42L (-17L)

(* Float vectors using bit patterns *)
let float_vec_zero = float64x2_of_int64s 0L 0L

let float_vec_ones =
  float64x2_of_int64s 0x3FF0000000000000L 0x3FF0000000000000L (* 1.0, 1.0 *)
;;

let float_vec_mixed =
  float64x2_of_int64s 0x4045400000000000L 0xC031400000000000L (* 42.5, -17.25 *)
;;

(* Functions to print vector components *)
let print_int64x2 vec =
  let low = int64x2_low_int64 vec in
  let high = int64x2_high_int64 vec in
  Printf.printf "[%Ld, %Ld]" low high
;;

let print_float64x2 vec =
  let low = float64x2_low_int64 vec in
  let high = float64x2_high_int64 vec in
  let low_float = Int64.float_of_bits low in
  let high_float = Int64.float_of_bits high in
  Printf.printf "[%.6f, %.6f]" low_float high_float
;;
