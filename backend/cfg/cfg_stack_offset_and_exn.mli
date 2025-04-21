[@@@ocaml.warning "+a-4-9-40-41-42"]

(** Compute [stack_offset] of all blocks and instructions and set [exn] field of all
    blocks. Assumes that the input Cfg has all [stack_offset] fields set to
    [Cfg.invalid_stack_offset] and all blocks have [exn] set to [None]. Used during Cfg
    construction. *)
val compute : Cfg.t -> unit
