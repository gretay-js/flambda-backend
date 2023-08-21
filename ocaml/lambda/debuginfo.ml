(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open Lexing
open Location

module Scoped_location = struct
  type scope_item =
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition
    | Sc_partial_or_eta_wrapper
    | Sc_lazy

  type scopes =
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string; name : string; prev: scopes;
               assume_zero_alloc: bool}

  let str = function
    | Empty -> ""
    | Cons r -> r.str

  let str_fun = function
    | Empty -> "(fun)"
    | Cons r -> r.str_fun

  let cons scopes item str name ~assume_zero_alloc =
    Cons {item; str; str_fun = str ^ ".(fun)"; name; prev = scopes;
          assume_zero_alloc}

  let empty_scopes = Empty

  let add_parens_if_symbolic = function
    | "" -> ""
    | s ->
       match s.[0] with
       | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> s
       | _ -> "(" ^ s ^ ")"

  let dot ?(sep = ".") ?no_parens scopes s =
    let s =
      match no_parens with
      | None -> add_parens_if_symbolic s
      | Some () -> s
    in
    match scopes with
    | Empty -> s
    | Cons {str; _} -> str ^ sep ^ s

  let enter_anonymous_function ~scopes ~assume_zero_alloc =
    let str = str_fun scopes in
    Cons {item = Sc_anonymous_function; str; str_fun = str; name = ""; prev = scopes;
          assume_zero_alloc }

  let enter_value_definition ~scopes ~assume_zero_alloc id =
    cons scopes Sc_value_definition (dot scopes (Ident.name id)) (Ident.name id)
      ~assume_zero_alloc

  let enter_compilation_unit ~scopes cu =
    let name = Compilation_unit.name_as_string cu in
    cons scopes Sc_module_definition (dot scopes name) name
      ~assume_zero_alloc:false

  let enter_module_definition ~scopes id =
    cons scopes Sc_module_definition (dot scopes (Ident.name id)) (Ident.name id)
      ~assume_zero_alloc:false

  let enter_class_definition ~scopes id =
    cons scopes Sc_class_definition (dot scopes (Ident.name id)) (Ident.name id)
      ~assume_zero_alloc:false

  let enter_method_definition ~scopes (s : Asttypes.label) =
    let str =
      match scopes with
      | Cons {item = Sc_class_definition; _} -> dot ~sep:"#" scopes s
      | _ -> dot scopes s
    in
    cons scopes Sc_method_definition str s ~assume_zero_alloc:false

  let enter_lazy ~scopes = cons scopes Sc_lazy (str scopes) ""
                             ~assume_zero_alloc:false

  let enter_partial_or_eta_wrapper ~scopes =
    cons scopes Sc_partial_or_eta_wrapper (dot ~no_parens:() scopes "(partial)") ""
      ~assume_zero_alloc:false

  let set_assume_zero_alloc ~scopes =
    match scopes with
    | Empty -> Empty
    | Cons { assume_zero_alloc = true } -> scopes
    | Cons { item; str; str_fun; name; prev; assume_zero_alloc = false; } ->
      Cons { item; str; str_fun; name; prev; assume_zero_alloc = true; }

  let get_assume_zero_alloc ~scopes =
    match scopes with
    | Empty -> false
    | Cons { assume_zero_alloc; _ } -> assume_zero_alloc

  let string_of_scopes = function
    | Empty -> "<unknown>"
    | Cons {str; assume_zero_alloc; _} ->
      if assume_zero_alloc then str^"(assume zero_alloc)"
      else str

  let string_of_scopes =
    let module StringSet = Set.Make (String) in
    let repr = ref StringSet.empty in
    fun scopes ->
      let res = string_of_scopes scopes in
      match StringSet.find_opt res !repr with
      | Some x -> x
      | None ->
        repr := StringSet.add res !repr;
        res

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  let of_location ~scopes loc =
    if Location.is_none loc then
      Loc_unknown
    else
      Loc_known { loc; scopes }

  let to_location = function
    | Loc_unknown -> Location.none
    | Loc_known { loc; _ } -> loc

  let string_of_scoped_location = function
    | Loc_unknown -> "??"
    | Loc_known { loc = _; scopes } -> string_of_scopes scopes

  let map_scopes f t =
    match t with
    | Loc_unknown -> Loc_unknown
    | Loc_known { loc; scopes } -> Loc_known { loc; scopes = f ~scopes }
end

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
}

type t = { dbg : item list; assume_zero_alloc : bool }

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_dbg : t }
type alloc_dbginfo = alloc_dbginfo_item list

let none = { dbg = []; assume_zero_alloc = false }

let is_none { dbg; assume_zero_alloc } =
  if assume_zero_alloc then false else
  match dbg with
  | [] -> true
  | _ :: _ -> false

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
    let items =
      List.map
        (fun d ->
           Printf.sprintf "%s:%d,%d-%d"
             d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
        ds
    in
    "{" ^ String.concat ";" items ^ "}"

let to_string { dbg; assume_zero_alloc; } =
  let s = to_string dbg in
  if assume_zero_alloc then s^"(assume zero_alloc)" else s

let item_from_location ~scopes loc =
  let valid_endpos =
    String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname in
  { dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if valid_endpos
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_start_bol = loc.loc_start.pos_bol;
    dinfo_end_bol =
      if valid_endpos then loc.loc_end.pos_bol
      else loc.loc_start.pos_bol;
    dinfo_end_line =
      if valid_endpos then loc.loc_end.pos_lnum
      else loc.loc_start.pos_lnum;
    dinfo_scopes = scopes
  }

let from_location = function
  | Scoped_location.Loc_unknown -> { dbg = []; assume_zero_alloc = false; }
  | Scoped_location.Loc_known {scopes; loc} ->
    assert (not (Location.is_none loc));
    let assume_zero_alloc = Scoped_location.get_assume_zero_alloc ~scopes in
    { dbg = [item_from_location ~scopes loc]; assume_zero_alloc; }

let to_location { dbg; assume_zero_alloc=_ } =
  match dbg with
  | [] -> Location.none
  | d :: _ ->
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = d.dinfo_start_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_start;
      } in
    let loc_end =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_end_line;
        pos_bol = d.dinfo_end_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_end;
      } in
    { loc_ghost = false; loc_start; loc_end; }

let inline { dbg = dbg1; assume_zero_alloc = a1; }
      { dbg = dbg2; assume_zero_alloc = a2; } =
  { dbg = dbg1 @ dbg2; assume_zero_alloc = a1 || a2; }

(* CR-someday afrisch: FWIW, the current compare function does not seem very
   good, since it reverses the two lists. I don't know how long the lists are,
   nor if the specific currently implemented ordering is useful in other
   contexts, but if one wants to use Map, a more efficient comparison should
   be considered. *)
let compare_dbg dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match ds1, ds2 with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
      let c = String.compare d1.dinfo_file d2.dinfo_file in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_line d2.dinfo_line in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_char_end d2.dinfo_char_end in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_char_start d2.dinfo_char_start in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_start_bol d2.dinfo_start_bol in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_end_bol d2.dinfo_end_bol in
      if c <> 0 then c else
      let c = Int.compare d1.dinfo_end_line d2.dinfo_end_line in
      if c <> 0 then c else
      loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)

let compare { dbg = dbg1; assume_zero_alloc = a1; }
      { dbg = dbg2; assume_zero_alloc = a2; } =
  let res = compare_dbg dbg1 dbg2 in
  if res <> 0 then res else Bool.compare a1 a2

let hash { dbg; assume_zero_alloc } =
  let init = Bool.to_int assume_zero_alloc in
  List.fold_left (fun hash item -> Hashtbl.hash (hash, item)) init dbg

let rec print_compact ppf t =
  let print_item item =
    Format.fprintf ppf "%a:%i"
      Location.print_filename item.dinfo_file
      item.dinfo_line;
    if item.dinfo_char_start >= 0 then begin
      Format.fprintf ppf ",%i--%i" item.dinfo_char_start item.dinfo_char_end
    end
  in
  match t with
  | [] -> ()
  | [item] -> print_item item
  | item::t ->
    print_item item;
    Format.fprintf ppf ";";
    print_compact ppf t

let print_compact ppf { dbg; } = print_compact ppf dbg

let to_list { dbg; } = dbg

let length { dbg; } = List.length dbg


let merge ~into:{ dbg = dbg1 } { dbg = _dbg2; } =
  { dbg = dbg1 }

let assume_zero_alloc t = t.assume_zero_alloc

