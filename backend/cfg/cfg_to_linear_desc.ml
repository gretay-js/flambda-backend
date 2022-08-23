open Cfg_intf.S

let from_basic (basic : basic) : Linear.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Move -> Lop Imove
  | Spill -> Lop Ispill
  | Reload -> Lop Ireload
  | Const_int n -> Lop (Iconst_int n)
  | Const_float n -> Lop (Iconst_float n)
  | Const_symbol n -> Lop (Iconst_symbol n)
  | Stackoffset n -> Lop (Istackoffset n)
  | Load (c, m, i) -> Lop (Iload (c, m, i))
  | Store (c, m, b) -> Lop (Istore (c, m, b))
  | Intop op -> Lop (Iintop op)
  | Intop_imm (op, i) -> Lop (Iintop_imm (op, i))
  | Negf -> Lop Inegf
  | Absf -> Lop Iabsf
  | Addf -> Lop Iaddf
  | Subf -> Lop Isubf
  | Mulf -> Lop Imulf
  | Divf -> Lop Idivf
  | Compf c -> Lop (Icompf c)
  | Floatofint -> Lop Ifloatofint
  | Intoffloat -> Lop Iintoffloat
  | Probe_is_enabled { name } -> Lop (Iprobe_is_enabled { name })
  | Opaque -> Lop Iopaque
  | Specific op -> Lop (Ispecific op)
  | Begin_region -> Lop Ibeginregion
  | End_region -> Lop Iendregion
  | Name_for_debugger { ident; which_parameter; provenance; is_assignment }
    ->
    Lop (Iname_for_debugger { ident; which_parameter; provenance; is_assignment })
