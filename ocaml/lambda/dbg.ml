open! Int_replace_polymorphic_compare

type item = {
  dinfo_file : string;
  dinfo_line : int;
  dinfo_char_start : int;
  dinfo_char_end : int;
  dinfo_start_bol : int;
  dinfo_end_bol : int;
  dinfo_end_line : int;
  dinfo_scopes : Scoped_location.scopes;
}

type t = item list

(* CR-someday afrisch: FWIW, the current compare function does not seem very
   good, since it reverses the two lists. I don't know how long the lists are,
   nor if the specific currently implemented ordering is useful in other
   contexts, but if one wants to use Map, a more efficient comparison should
   be considered. *)
let compare dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match (ds1, ds2) with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
        let c = String.compare d1.dinfo_file d2.dinfo_file in
        if c <> 0 then
          c
        else
          let c = Int.compare d1.dinfo_line d2.dinfo_line in
          if c <> 0 then
            c
          else
            let c = Int.compare d1.dinfo_char_end d2.dinfo_char_end in
            if c <> 0 then
              c
            else
              let c = Int.compare d1.dinfo_char_start d2.dinfo_char_start in
              if c <> 0 then
                c
              else
                let c = Int.compare d1.dinfo_start_bol d2.dinfo_start_bol in
                if c <> 0 then
                  c
                else
                  let c = Int.compare d1.dinfo_end_bol d2.dinfo_end_bol in
                  if c <> 0 then
                    c
                  else
                    let c = Int.compare d1.dinfo_end_line d2.dinfo_end_line in
                    if c <> 0 then
                      c
                    else
                      loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)

let is_none dbg =
  match dbg with
  | [] -> true
  | _ :: _ -> false

let hash dbg = List.fold_left (fun hash item -> Hashtbl.hash (hash, item)) 0 dbg

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
      let items =
        List.map
          (fun d ->
            Printf.sprintf "%s:%d,%d-%d" d.dinfo_file d.dinfo_line
              d.dinfo_char_start d.dinfo_char_end
          )
          ds
      in
      "{" ^ String.concat ";" items ^ "}"

let to_list t = t
let length t = List.length t

let rec print_compact ppf t =
  let print_item (item : item) =
    Format.fprintf ppf "%a:%i" Location.print_filename item.dinfo_file
      item.dinfo_line;
    if item.dinfo_char_start >= 0 then
      Format.fprintf ppf ",%i--%i" item.dinfo_char_start item.dinfo_char_end
  in
  match t with
  | [] -> ()
  | [ item ] -> print_item item
  | item :: t ->
      print_item item;
      Format.fprintf ppf ";";
      print_compact ppf t
