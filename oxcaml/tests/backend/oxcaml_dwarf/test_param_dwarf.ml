module T1 = struct
  let[@inline never] [@local never] test x y =
    if x > 0 then print_int x else print_float y;
    print_newline ()
  ;;
end

let () =
  T1.test (Sys.opaque_identity 1) (Sys.opaque_identity 1.0);
  T1.test (Sys.opaque_identity (-1)) (Sys.opaque_identity (-1.0));
  ()
;;
