(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda
open Location


let is_inline_attribute =
  [ ["inline"; "ocaml.inline"],true ]

let is_inlined_attribute =
  [ ["inlined"; "ocaml.inlined"], true
  ; ["unrolled"; "ocaml.unrolled"], (Config.flambda || Config.flambda2)
  ]

let is_specialise_attribute =
  [ ["specialise"; "ocaml.specialise"], Config.flambda ]

let is_specialised_attribute =
  [ ["specialised"; "ocaml.specialised"], Config.flambda ]

let is_local_attribute =
  [ ["local"; "ocaml.local"], true ]

let is_tailcall_attribute =
  [ ["tailcall"; "ocaml.tailcall"], true ]

let is_tmc_attribute =
  [ ["tail_mod_cons"; "ocaml.tail_mod_cons"], true ]

let is_poll_attribute =
  [ ["poll"; "ocaml.poll"], true ]

let is_loop_attribute =
  [ ["loop"; "ocaml.loop"], true ]

let is_zero_alloc_attribute =
  [ ["zero_alloc"; "ocaml.zero_alloc"], true ]

let find_attribute p attributes =
  let inline_attribute = Builtin_attributes.filter_attributes p attributes in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | attr :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some attr
  in
  attr

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false

let get_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [{pstr_desc = Pstr_eval (exp, [])}] -> get_from_exp exp
  | _ -> Result.Error ()

let get_optional_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [] -> Result.Ok None
  | other -> Result.map Option.some (get_payload get_from_exp other)

let get_id_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok id
  | _ -> Result.Error ()

let get_int_from_exp =
  let open Parsetree in
  function
    | { pexp_desc = Pexp_constant (Pconst_integer(s, None)) } ->
        begin match Misc.Int_literal_converter.int s with
        | n -> Result.Ok n
        | exception (Failure _) -> Result.Error ()
        end
    | _ -> Result.Error ()

let get_construct_from_exp =
  let open Parsetree in
  function
    | { pexp_desc =
          Pexp_construct ({ txt = Longident.Lident constr }, None) } ->
        Result.Ok constr
    | _ -> Result.Error ()

let get_bool_from_exp exp =
  Result.bind (get_construct_from_exp exp)
    (function
      | "true" -> Result.Ok true
      | "false" -> Result.Ok false
      | _ -> Result.Error ())

let parse_id_payload txt loc ~default ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    default
  in
  match get_optional_payload get_id_from_exp payload with
  | Error () -> warn ()
  | Ok None -> empty
  | Ok (Some id) ->
      match List.assoc_opt id cases with
      | Some r -> r
      | None -> warn ()

let parse_inline_attribute attr : inline_attribute =
  match attr with
  | None -> Default_inline
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inline
    end else
      parse_id_payload txt loc
        ~default:Default_inline
        ~empty:Always_inline
        [
          "never", Never_inline;
          "always", Always_inline;
          "available", Available_inline;
        ]
        payload

let parse_inlined_attribute attr : inlined_attribute =
  match attr with
  | None -> Default_inlined
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inlined
    end else
      parse_id_payload txt loc
        ~default:Default_inlined
        ~empty:Always_inlined
        [
          "never", Never_inlined;
          "always", Always_inlined;
          "hint", Hint_inlined;
        ]
        payload

let parse_specialise_attribute attr =
  match attr with
  | None -> Default_specialise
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_specialise
        ~empty:Always_specialise
        [
          "never", Never_specialise;
          "always", Always_specialise;
        ]
        payload

let parse_local_attribute attr =
  match attr with
  | None -> Default_local
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_local
        ~empty:Always_local
        [
          "never", Never_local;
          "always", Always_local;
          "maybe", Default_local;
        ]
        payload

let parse_poll_attribute attr =
  match attr with
  | None -> Default_poll
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_poll
        ~empty:Default_poll
        [
          "error", Error_poll;
        ]
        payload

let parse_loop_attribute attr =
  match attr with
  | None -> Default_loop
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_loop
        ~empty:Always_loop
        [
          "never", Never_loop;
          "always", Always_loop;
        ]
        payload

let get_inline_attribute l =
  let attr = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let get_specialise_attribute l =
  let attr = find_attribute is_specialise_attribute l in
  parse_specialise_attribute attr

let get_local_attribute l =
  let attr = find_attribute is_local_attribute l in
  parse_local_attribute attr

let get_poll_attribute l =
  let attr = find_attribute is_poll_attribute l in
  parse_poll_attribute attr

let get_loop_attribute l =
  let attr = find_attribute is_loop_attribute l in
  parse_loop_attribute attr

let check_local_inline loc attr =
  match attr.local, attr.inline with
  | Always_local, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Duplicated_attribute "local/inline")
  | _ ->
      ()

let check_poll_inline loc attr =
  match attr.poll, attr.inline with
  | Error_poll, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with inlining")
  | _ ->
      ()

let check_poll_local loc attr =
  match attr.poll, attr.local with
  | Error_poll, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with local function optimization")
  | _ ->
      ()

let lfunction_with_attr ~attr
  { kind; params; return; body; attr=_; loc; mode; region } =
  lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~region

let add_inline_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_inline_attribute attributes with
      | Default_inline -> expr
      | (Always_inline | Available_inline | Never_inline | Unroll _)
          as inline ->
        begin match attr.inline with
          | Default_inline -> ()
          | Always_inline | Available_inline | Never_inline | Unroll _ ->
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "inline")
        end;
        let attr = { attr with inline } in
        check_local_inline loc attr;
        check_poll_inline loc attr;
        lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_specialise_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_specialise_attribute attributes with
    | Default_specialise -> expr
    | (Always_specialise | Never_specialise) as specialise ->
      begin match attr.specialise with
      | Default_specialise -> ()
      | Always_specialise | Never_specialise ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "specialise")
      end;
      let attr = { attr with specialise } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_local_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_local_attribute attributes with
    | Default_local -> expr
    | (Always_local | Never_local) as local ->
      begin match attr.local with
      | Default_local -> ()
      | Always_local | Never_local ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "local")
      end;
      let attr = { attr with local } in
      check_local_inline loc attr;
      check_poll_local loc attr;
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let duplicated_attribute_warning loc old_check new_check =
    match old_check, new_check with
    | Default_check, _
    | _, Default_check -> ()
    | (Check { property = p; assume = a; strict  = s }),
      (Check { property = p'; assume = a'; strict = s' }) ->
      if p = p' &&  not ( (a = a') && (s = s') ) then
        Location.prerr_warning loc
          (Warnings.Duplicated_attribute
             (Printlambda.property_to_string p))
    | Ignore_assert_all _, _
    | _, Ignore_assert_all _ -> assert false

let misplaced_assume_warning attr loc txt =
  (* [attr.inline] and [attr.specialise] must be set before the
     check for [Warnings.Misplaced_assume_attribute].
     For attributes from the same list, it's fine because
     [add_check_attribute] is called after
     [add_inline_attribute] and [add_specialise_attribute].
     The warning will spuriously fire in the following case:
     let[@inline never][@specialise never] f =
     fun[@zero_alloc assume] x -> ..
  *)
  match attr.check with
  | Check { assume = false }
  | Default_check
  | Ignore_assert_all _ -> ()
  | Check { assume = true; _ } ->
    let never_specialise =
      if Config.flambda then
        attr.specialise = Never_specialise
      else
        (* closure drops [@specialise never] and never specialises *)
        (* flambda2 does not have specialisation support yet *)
        true
    in
    if not ((attr.inline = Never_inline) &&
            never_specialise &&
            (attr.local = Never_local) then
      Location.prerr_warning loc
        (Warnings.Misplaced_assume_attribute txt)

let add_check_attribute expr floc _attributes warnings =
  match expr with
  | Lfunction({ attr = { stub = false }; } as funct) ->
    (match warnings with
     | None -> expr
     | Some warnings ->
       let check = Warnings.get_checks warnings in
       let property = Zero_alloc in
       let txt = Printlambda.property_to_string property in
       let check_if_active w strict loc =
         if Warnings.is_active_in_state w warnings then begin
           (* There will be no warning for misplaced or unused "zero_alloc"
              annotations. The warning for unchecked function will only trigger when
              correctly annotated function is optimized away. *)
           Builtin_attributes.register_property { txt; loc };
           Check { assume = false; property; strict; loc; }
         end else
           Default_check
       in
       let check =
         (match check with
          | Off -> Default_check
          | Assume { strict; loc } ->
            Check { assume = true; property; strict; loc; }
          | On { strict; loc } ->
            check_if_active (Warnings.Check_failed ("", [])) strict loc
          | Opt { strict; loc } ->
            check_if_active (Warnings.Check_failed_opt ("", [])) strict loc)
       in
       duplicated_attribute_warning floc funct.attr.check check;
       let attr = { funct.attr with check } in
       lfunction_with_attr ~attr funct)
  | expr -> expr


let parse attributes floc =
  let f a =
    match Builtin_attributes.process_check_attribute ~direct:true p attr with
    | None -> None
    | Some c -> a,c
    match
  in
  let p = Warnings.Check.Zero_alloc in
  attributes
  |> Builtin_attributes.filter_attributes is_zero_alloc_attribute
  |> List.filter_map f
  |> (function
    | [] -> None
    | [attr,c] -> Some c
    | attr, c :: {Parsetree.attr_name = {txt;loc}; _}, _ :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some c

  (let p = Warnings.Checks.Zero_alloc in
     match parse_check_attribute_payload attr p with
     | None -> ()
     | Some { scope = Direct; } ->
       (* this attribute is consumed at lambda or
          triggers an misplaced attribute warning. *)
       ()
     | Some ({ scope = All | Toplevel; } as c) ->
       mark_used attr.attr_name;
       Warnings.set_checks c)


  match find_attribute attributes is_zero_alloc_attribute with
  | Some



let add_check_attribute expr ~warnings ~in_structure floc attributes =
  let update check scoped =
    match check.scoped.scope, new_scoped.scope with
    | Direct, (All | Toplevel) -> assert false
    | (All | Toplevel), Direct -> { check with scoped }
    | Direct, Direct ->
      (match check.scoped.state, scoped.state with
       | Off, Off -> ()
       | On { strict=s; opt=o; }, On { strict=s'; opt=s' } ->
         if not (s = s' && o = o') then
           duplicate warning
       | Off { strict=s; never_returns_normally=

        { check with scoped }
      )
  in
  match expr with
  | Lfunction({ attr = { stub = false; check; }; _ } as funct) ->
    let check =
      match warnings, in_structure with
      | Some warnings, None ->
        assert (Option.is_none check.in_structure);
        update check (Warnings.get_checks warnings)
      | None, Some in_structure ->
        (match check.in_structure with
         | None -> { check with in_structure = Some in_structure }
         | Some b ->
           assert (b = in_structure);
           check);
      | None, None | Some _, Some _ -> assert false
    in
    let check =
      match parse attributes floc with
      | None -> check
      | Some scoped -> update check scoped
    in
    let attr = { attr with check } in
    lfunction { funct with atttr }
  | expr -> expr

let add_loop_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_loop_attribute attributes with
    | Default_loop -> expr
    | (Always_loop | Never_loop) as loop ->
      begin match attr.loop with
      | Default_loop -> ()
      | Always_loop | Never_loop ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "loop")
      end;
      let attr = { attr with loop } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_tmc_attribute expr loc attributes =
  match expr with
  | Lfunction funct ->
     let attr = find_attribute is_tmc_attribute attributes in
     begin match attr with
     | None -> expr
     | Some _ ->
        if funct.attr.tmc_candidate then
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "tail_mod_cons");
        let attr = { funct.attr with tmc_candidate = true } in
        lfunction_with_attr ~attr funct
     end
  | _ -> expr

let add_poll_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_poll_attribute attributes with
    | Default_poll -> expr
    | Error_poll as poll ->
      begin match attr.poll with
      | Default_poll -> ()
      | Error_poll ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "poll error")
      end;
      let attr = { attr with poll } in
      check_poll_inline loc attr;
      check_poll_local loc attr;
      let attr = { attr with inline = Never_inline; local = Never_local } in
      lfunction_with_attr ~attr funct
    end
  | expr -> expr

(* Get the [@inlined] attribute payload (or default if not present). *)
let get_inlined_attribute e =
  let attr = find_attribute is_inlined_attribute e.exp_attributes in
  parse_inlined_attribute attr

let get_inlined_attribute_on_module e =
  let rec get mod_expr =
    let attr = find_attribute is_inlined_attribute mod_expr.mod_attributes in
    let attr = parse_inlined_attribute attr in
    let attr =
      match mod_expr.Typedtree.mod_desc with
      | Tmod_constraint (me, _, _, _) ->
        let inner_attr = get me in
        begin match attr with
        | Always_inlined | Hint_inlined | Never_inlined | Unroll _ -> attr
        | Default_inlined -> inner_attr
        end
      | _ -> attr
    in
    attr
  in
  get e

let get_specialised_attribute e =
  let attr = find_attribute is_specialised_attribute e.exp_attributes in
  parse_specialise_attribute attr

let get_tailcall_attribute e =
  let attr = find_attribute is_tailcall_attribute e.exp_attributes in
  match attr with
  | None -> Default_tailcall
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
    match get_optional_payload get_bool_from_exp payload with
    | Ok (None | Some true) -> Tailcall_expectation true
    | Ok (Some false) -> Tailcall_expectation false
    | Error () ->
        let msg = "Only an optional boolean literal is supported." in
        Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
        Default_tailcall

let add_function_attributes lam warnings loc attr =
  let lam =
    add_inline_attribute lam loc attr
  in
  let lam =
    add_specialise_attribute lam loc attr
  in
  let lam =
    add_local_attribute lam loc attr
  in
  let lam =
    add_check_attribute lam loc attr warnings
  in
  let lam =
    add_loop_attribute lam loc attr
  in
  let lam =
    add_tmc_attribute lam loc attr
  in
  let lam =
    (* last because poll overrides inline and local *)
    add_poll_attribute lam loc attr
  in
  lam
