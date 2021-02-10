# 1 "stdLabels.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Standard labeled libraries.

   This meta-module provides labelized version of the {!Array},
   {!Bytes}, {!List} and {!String} modules.

   They only differ by their labels. Detailed interfaces can be found
   in [arrayLabels.mli], [bytesLabels.mli], [listLabels.mli]
   and [stringLabels.mli].
*)

[@@@ocaml.nolabels]

module Array = ArrayLabels
module Bytes = BytesLabels
module List = ListLabels
module String = StringLabels
