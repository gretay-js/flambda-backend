type t = float

external c_round : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_round"
  [@@noalloc]

external c_min : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_min"
  [@@noalloc]

external c_max : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_max"
  [@@noalloc]

external c_sqrt : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_sqrt"
  [@@noalloc]

let check_floats f =
  let open Float in
  Random.set_state (Random.State.make [| 1234567890 |]);
  f zero zero;
  f zero one;
  f one one;
  f zero minus_one;
  f minus_one minus_one;
  f one minus_one;
  f zero (-0.0);
  f nan zero;
  f infinity zero;
  f neg_infinity zero;
  f nan nan;
  f infinity infinity;
  f neg_infinity neg_infinity;
  f neg_infinity infinity;
  f infinity nan;
  f neg_infinity nan;
  f max_float infinity;
  f max_float neg_infinity;
  f min_float infinity;
  f min_float neg_infinity;
  f max_float max_float;
  f min_float min_float;
  f max_float min_float;
  for _ = 0 to 100_000 do
    let f0 = Random.int64 Int64.max_int in
    let f1 = Random.int64 Int64.max_int in
    f
      (if Random.bool ()
      then Int64.float_of_bits f0
      else Int64.(neg f0 |> float_of_bits))
      (if Random.bool ()
      then Int64.float_of_bits f1
      else Int64.(neg f1 |> float_of_bits))
  done

module Tests = struct
  include Builtins.Float64

  let () =
    check_floats (fun l r -> eqf' (max l r) (c_max l r));
    check_floats (fun l r -> eqf' (min l r) (c_min l r));
    check_floats (fun l _ -> eqf' (sqrt l) (c_sqrt l));
    check_floats (fun l _ -> eqf' (round_nearest l) (c_round l))
end
