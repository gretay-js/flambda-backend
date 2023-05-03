let () =
  let enabled_if flambda_only =
    if flambda_only then
      (* what we really want to say if dune knew about flambda2:
         (or %{ocaml-config:flambda} %{ocaml-config:flambda2}) *)
      {|(enabled_if (and (= %{context_name} "main") %{ocaml-config:flambda}))|}
    else
      {|(enabled_if (= %{context_name} "main"))|}
  in
  let print_test ~flambda_only ~deps =
    let enabled_if = enabled_if flambda_only in
    Printf.printf
    {|
(rule
 (alias   runtest)
 %s
 (deps %s)
 (action (run %%{bin:ocamlopt.opt} %%{deps} -g -c -zero-alloc-check -dcse -dcheckmach -dump-into-file -O3)))
|}
    enabled_if deps
  in
  let print_test_expected_output ~flambda_only ~extra_dep ~exit_code name =
    let enabled_if = enabled_if flambda_only in
    let ml_deps =
      let s =
        match extra_dep with
        | None -> ""
        | Some s -> s^" "
      in
      Printf.sprintf {|(:ml %s%s.ml)|} s name
    in
  Printf.printf
    {|
(rule
 %s
 (targets %s.output.corrected)
 (deps %s filter.sh)
 (action
   (with-outputs-to %s.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes %d
     (run %%{bin:ocamlopt.opt} %%{ml} -g -color never -error-style short -c -zero-alloc-check -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 %s
 (deps %s.output %s.output.corrected)
 (action (diff %s.output %s.output.corrected)))
|}
  enabled_if name ml_deps name exit_code enabled_if name name name name
  in
  print_test ~flambda_only:false ~deps:"s.ml t.ml";
  print_test ~flambda_only:false ~deps:"t5.ml test_assume.ml";
  print_test ~flambda_only:true ~deps:"test_flambda.ml";
  for i = 1 to 15 do
    let extra_dep =
      match i with
      | 3 | 4 -> Some (Printf.sprintf "t%d.ml" i)
      | _ -> None
    in
    let flambda_only =
      match i with
      | 12 -> true
      | _ -> false
    in
    let name = Printf.sprintf "fail%d" i in
    print_test_expected_output ~flambda_only ~extra_dep ~exit_code:2 name
  done;
  print_test_expected_output ~flambda_only:false ~extra_dep:None
    ~exit_code:2 "test_attribute_error_duplicate";
  (* Closure does not optimize the function away, so the unchecked attribute
     warning is only with flambda and flambda2. *)
  print_test_expected_output ~flambda_only:true ~extra_dep:None
    ~exit_code:0 "test_attr_unused";
  (* Checks that the warning is printed and compilation is successful. *)
  print_test_expected_output ~flambda_only:false ~extra_dep:None
    ~exit_code:0 "t6"
