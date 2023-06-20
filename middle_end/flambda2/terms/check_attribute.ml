(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Warnings.Checks.t

let print ppf t = Warnings.Checks.print ppf t

let from_lambda t = t

let equal x y = Warnings.Checks.equal x y

let default = Warnings.Checks.default

let is_default t = (t = Warnings.Checks.default)
