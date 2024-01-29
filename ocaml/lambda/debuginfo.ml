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

type t = { dbg : Dbg.t; assume_zero_alloc : Assume_info.t }

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_dbg : t }
type alloc_dbginfo = alloc_dbginfo_item list

let none = { dbg = []; assume_zero_alloc = Assume_info.none }

let to_string { dbg; assume_zero_alloc; } =
  let s = Dbg.to_string dbg in
  let a = Assume_info.to_string assume_zero_alloc in
  s^a

let item_from_location ~scopes loc : Dbg.item =
  let valid_endpos =
    String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname in
  { Dbg.dinfo_file = loc.loc_start.pos_fname;
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
  | Scoped_location.Loc_unknown -> { dbg = []; assume_zero_alloc = Assume_info.none; }
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
  { dbg = dbg1 @ dbg2; assume_zero_alloc = Assume_info.join a1 a2; }

let is_none { dbg; assume_zero_alloc } =
  (not Assume_info.(equal assume_zero_alloc Assume_info.none)) && Dbg.is_none dbg

let compare { dbg = dbg1; assume_zero_alloc = a1; }
      { dbg = dbg2; assume_zero_alloc = a2; } =
  let res = Dbg.compare dbg1 dbg2 in
  if res <> 0 then res else Assume_info.compare a1 a2

let print_compact ppf { dbg; } = Dbg.print_compact ppf dbg

let merge ~into:{ dbg = dbg1; assume_zero_alloc = a1; }
      { dbg = dbg2; assume_zero_alloc = a2 } =
  (* Keep the first [dbg] info to match existing behavior.
     When assume_zero_alloc is only on one of the inputs but not both, keep [dbg]
     from the other.
  *)
  let dbg =
    match Assume_info.is_none a1, Assume_info.is_none a2 with
    | false, true -> dbg2
    | _,  _ -> dbg1
  in
  { dbg;
    assume_zero_alloc = Assume_info.meet a1 a2
  }

let assume_zero_alloc t = t.assume_zero_alloc

let get_dbg t = t.dbg

