(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



val pretty_const
  : Typedtree.constant -> string
val pretty_val : Format.formatter -> 'k Typedtree.general_pattern -> unit

val pretty_pat
    : Format.formatter -> 'k Typedtree.general_pattern -> unit
val pretty_line
    : Format.formatter -> 'k Typedtree.general_pattern list -> unit
val pretty_matrix
    : Format.formatter -> 'k Typedtree.general_pattern list list -> unit
