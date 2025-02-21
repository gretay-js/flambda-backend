let check_index first last index =
  if index < first || index > last then
    Misc.fatal_errorf "Illegal register index %d" index ();

(* Float/SIMD register description *)
module Neon_reg_name = struct

  module Vector = struct
    type t =
      | V8B
      | V16B
      | V4H
      | V8H
      | V2S
      | V4S
      | V1D
      | V2D

    let to_string t =
      match t with
      | V8B -> "8B"
      | V16B -> "16B"
      | V4H -> "4H"
      | V8H -> "8H"
      | V2S -> "2S"
      | V4S -> "4S"
      | V1D -> "1D"
      | V2D -> "2D"

    let name t index =
      Printf.sprintf "V%d.%s" index (to_string t)
  end

  module Scalar = struct
    type t =
      | B
      | H
      | S
      | D
      | Q

    let to_string t =
      match t with
      | B -> "B"
      | H -> "H"
      | S -> "S"
      | D -> "D"
      | Q -> "Q"

    let name t index =
      Printf.sprintf "%s%d" (to_string t) index

  end

  type t =
    | Vector of Vector.t
    | Scalar of Scalar.t

  let last = 31

  let check_index t index =
    check_index 0 last index

  let name t index =
    match t with
    | Vector v -> Vector.name v index
    | Scalar s -> Scalar.name s index
end

(* General-purpose register description *)
module GP_reg_name = struct
  type t =
    | W
    | X
    | WZR
    | XZR
    | WSP
    | SP

  let last_numbered = 30
  let last = 31

  let check_index t index =
    match t with
    | W
    | X -> check_index 0 last_numbered index
    | WZR
    | XZR
    | WSP
    | SP -> check_index last last index

  let name t index =
    match t with
    | W -> Printf.sprintf "W%d" index
    | X -> Printf.sprintf "X%d" index
    | WZR -> "WZR"
    | XZR -> "XZR"
    | WSP -> "WSP"
    | SP -> "SP"
end

(* Register representation *)
module Reg_name = struct
  type t =
    | GP of GP_reg_name.t
    | Neon of Neon_reg_name.t

  let check_index t index =
    match t with
    | GP rn -> GP_reg_name.check_index rn index
    | Neon rn -> Neon_reg_name.check_index rn index

  let name t index =
    match t with
    | GP rn -> GP_reg_name.name rn index
    | Neon rn -> Neon_reg_name.name rn index
end

module Reg = struct
  type t = { reg_name:Reg_name.t; index:int }

  let create reg_name index =
    Reg_name.check_index reg_name index;
    { reg_name; index; }

  let name t =
    Reg_name.name t.reg_name t.index
end

module DSL = struct
  (* create common combinations ahead of time? *)
  let reg_v2s =
    let name = (Reg_name.Neon Neon_reg_name.(Vector V2S)) in
    Array.init Neon_reg_name.last (fun i ->  Reg.create name i)

  let reg_s =
    let name = (Reg_name.Neon Neon_reg_name.(Scalar S)) in
    Array.init Neon_reg_name.last (fun i -> Reg.create name i)

  let reg_x =
    let name = (Reg_name.GP GP_reg_name.X) in
    Array.init GP_reg_name.last (fun i -> Reg.create name i)

  let reg_v2s index = reg_v2s.(index)

  let reg_s index = reg_s.(index)

  let reg_x index = reg_x.(index)

  let reg_name r = Reg.name r
end
