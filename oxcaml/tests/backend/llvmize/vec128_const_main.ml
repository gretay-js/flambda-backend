(* Main entry point for vec128_const test *)

let () =
  (* Test Int64x2 constants *)
  print_endline "Int64x2 constants:";
  print_string "int_vec_zero: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_zero;
  print_newline ();
  print_string "int_vec_one: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_one;
  print_newline ();
  print_string "int_vec_ascending: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_ascending;
  print_newline ();
  print_string "int_vec_max: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_max;
  print_newline ();
  print_string "int_vec_min: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_min;
  print_newline ();
  print_string "int_vec_mixed: ";
  Vec128_const.print_int64x2 Vec128_const.int_vec_mixed;
  print_newline ();
  (* Test Float64x2 constants *)
  print_endline "";
  print_endline "Float64x2 constants:";
  print_string "float_vec_zero: ";
  Vec128_const.print_float64x2 Vec128_const.float_vec_zero;
  print_newline ();
  print_string "float_vec_ones: ";
  Vec128_const.print_float64x2 Vec128_const.float_vec_ones;
  print_newline ();
  print_string "float_vec_mixed: ";
  Vec128_const.print_float64x2 Vec128_const.float_vec_mixed;
  print_newline ()
;;
