(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
(** Aggregated decode execution profile for feedback-directed-optimization. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module String = Misc.Stdlib.String
module Int = Numbers.Int

module S = struct
  module Loc = struct
    type rel =
      { id : int;  (** Unique id of the containing function symbol *)
        offset : int  (** Offset from the start of the function *)
      }

    type t =
      { rel : rel option;
        dbg : int option  (** debug info: linearid *)
      }
  end

  module Raw_addr = struct
    include Int64

    include Identifiable.Make (struct
      type nonrec t = t

      let compare = Int64.compare

      let hash = Hashtbl.hash

      let equal = Int64.equal

      let print ppf t = Format.fprintf ppf "%Ld" t

      let output c t = Printf.fprintf c "%Ld" t
    end)
  end

  module Raw_addr_pair = struct
    type t = Raw_addr.t * Raw_addr.t

    include Identifiable.Make (Identifiable.Pair (Raw_addr) (Raw_addr))
  end

  module Execount = struct
    include Int64
  end

  module Aggregated_perf_profile = struct
    type t =
      { instructions : Execount.t Raw_addr.Tbl.t;
        branches : Execount.t Raw_addr_pair.Tbl.t;
            (** number of times the branch was taken. *)
        mispredicts : Execount.t Raw_addr_pair.Tbl.t;
            (** number of times the branch was mispredicted: branch target
                mispredicted or branch direction was mispredicted. *)
        traces : Execount.t Raw_addr_pair.Tbl.t;
            (** execution count: number of times the trace was taken. *)
        buildid : string option
            (** identifier of the relevant unit (i.e., binary's buildid or
                function's crc in the future), if known. *)
      }
  end

  module Func = struct
    type t =
      {  id : int  (** Unique identifier we assign to this function *) ;
         (** Raw start address of the function in original binary *)
        start : Raw_addr.t;
        finish : Raw_addr.t;
         (** Preliminary execution count *)
        count : Execount.t;
        (** Does the function have any linearids? *)
        has_linearids : bool;
        (** Counters that refer to this function, uses raw addresses. *)
        agg : Aggregated_perf_profile.t

      }
  end

  module Kind = struct
    type t =
      | Func
      | Unit
  end

  module IR = struct
    type t = Clflags.Compiler_ir.t =
      | Linear
      | Cfg
  end

  module Crcs = struct
    module Crc = struct
      type t =
        { kind : Kind.t;
          crc : Digest.t;
          ir : IR.t
        }
      [@@deriving sexp, equal, bin_io]
    end

    (** map name to the corresponding Crc *)
    type tbl = Crc.t String.Tbl.t
  end

  type t =
    { (* map raw addresses to locations *)
      addr2loc : Loc.t Raw_addr.Tbl.t;
      (* map func name to func id *)
      name2id : int String.Tbl.t;
      (* map func id to func info *)
      functions : Func.t Int.Tbl.t;
      (* map name of compilation unit or function to its md5 digest. Currently
         contains only crcs of linear IR. Not using Caml.Digest.t because it
         does not have sexp. Not using Core's Digest because digests generated
         by the compiler using Caml.Digest might disagree. *)
      crcs : Crcs.tbl;
      (* buildid of the executable, if known *)
      mutable buildid : string option
    }
end
