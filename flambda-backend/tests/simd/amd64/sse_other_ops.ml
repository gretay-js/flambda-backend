module Float32 = Float32_reference

module Float32x4 = struct
  include Builtins.Float32x4
  include Sse_other_builtins.Float32x4

  let () =
    Float32.check_floats (fun f0 f1 ->
        (failmsg
           := fun () ->
                Printf.printf "dpf32 %f %f\n%!" (Int32.float_of_bits f0)
                  (Int32.float_of_bits f1));
        let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
        let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
        let result = dp 0b1111_0001 fv0 fv1 in
        let expect =
          Float32.to_float32x4
            (Float32.add
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0))
               (Float32.add (Float32.mul f0 f1) (Float32.mul f1 f0)))
            0l 0l 0l
        in
        (* When both are NaN, AMD returns the first argument and Intel returns
           the second argument. Hence we do not test this case. *)
        if f0 |> Int32.float_of_bits |> Float.is_nan
           && f1 |> Int32.float_of_bits |> Float.is_nan
        then ()
        else eq_float32x4 ~result ~expect)

  let () =
    Test_helpers.run_if_not_under_rosetta2 ~f:(fun () ->
        Float32.check_floats (fun f0 f1 ->
            (failmsg
               := fun () ->
                    Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                      (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f0 f1 f1 in
            let fv1 = Float32.to_float32x4 f1 f1 f0 f0 in
            let result = addsub fv0 fv1 in
            let expect =
              Float32.to_float32x4 (Float32.sub f0 f1) (Float32.add f0 f1)
                (Float32.sub f1 f0) (Float32.add f1 f0)
            in
            eq_float32x4 ~result ~expect);
        Float32.check_floats (fun f0 f1 ->
            (failmsg
               := fun () ->
                    Printf.printf "%f | %f\n%!" (Int32.float_of_bits f0)
                      (Int32.float_of_bits f1));
            let fv0 = Float32.to_float32x4 f0 f1 f0 f1 in
            let fv1 = Float32.to_float32x4 f1 f0 f1 f0 in
            let result = hsub fv0 fv1 in
            let expect =
              Float32.to_float32x4 (Float32.sub f0 f1) (Float32.sub f0 f1)
                (Float32.sub f1 f0) (Float32.sub f1 f0)
            in
            eq_float32x4 ~result ~expect))
end
