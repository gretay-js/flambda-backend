[@@@ocaml.warning "+a-37-40-41-42"]

open! Int_replace_polymorphic_compare

let check_index first last index =
  if index < first || index > last
  then Misc.fatal_errorf "Illegal register index %d" index ()

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

    let name t index = Printf.sprintf "V%d.%s" index (to_string t)
  end

  module Scalar = struct
    type t =
      | B
      | H
      | S
      | D
      | Q

    let to_string t =
      match t with B -> "b" | H -> "h" | S -> "s" | D -> "d" | Q -> "q"

    let name t index = Printf.sprintf "%s%d" (to_string t) index
  end

  type t =
    | Vector of Vector.t
    | Scalar of Scalar.t

  let last = 31

  let check_index _t index = check_index 0 last index

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
    | W | X -> check_index 0 last_numbered index
    | WZR | XZR | WSP | SP -> check_index last last index

  let name t index =
    match t with
    | W -> Printf.sprintf "w%d" index
    | X -> Printf.sprintf "x%d" index
    | WZR -> "wzr"
    | XZR -> "xzr"
    | WSP -> "wsp"
    | SP -> "sp"
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
  type t =
    { reg_name : Reg_name.t;
      index : int
    }

  let create reg_name index =
    Reg_name.check_index reg_name index;
    { reg_name; index }

  let name t = Reg_name.name t.reg_name t.index

  (* preallocate the registers *)
  let reg_array ~last name = Array.init (last + 1) (fun i -> create name i)

  let reg_x_array =
    reg_array ~last:GP_reg_name.last_numbered (Reg_name.GP GP_reg_name.X)

  let reg_w_array =
    reg_array ~last:GP_reg_name.last_numbered (Reg_name.GP GP_reg_name.W)

  let reg_s_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar S))

  let reg_d_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar D))

  let reg_q_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar Q))

  let reg_v2d_array =
    reg_array ~last:Neon_reg_name.last
      (Reg_name.Neon (Neon_reg_name.Vector V2D))

  (* for special GP registers we use the last index *)
  let sp = create (GP SP) GP_reg_name.last

  let wsp = create (GP WSP) GP_reg_name.last

  let xzr = create (GP XZR) GP_reg_name.last

  let wzr = create (GP WZR) GP_reg_name.last

  let reg_x i = reg_x_array.(i)

  let reg_w i = reg_w_array.(i)

  let reg_s i = reg_s_array.(i)

  let reg_d i = reg_d_array.(i)

  let reg_q i = reg_q_array.(i)

  let reg_v2d i = reg_v2d_array.(i)
end
