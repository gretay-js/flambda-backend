(* Objcopy cmx file. *)

open Misc

let remove_check = ref false
let output_file = ref None

let dump_obj filename =
  let open Compilenv in
  let ui, _crc = read_unit_info filename in
  if !remove_check then
    Checks.reset ui.ui_checks;
  let output_filename =
    match !output_file with
    | None -> filename ^".updated-cmx"
    | Some f -> f
  in
  Compilenv.write_unit_info ui output_filename


let arg_list =
  [ ( "-remove-check",
      Arg.Set remove_check,
      " Remove summaries" );
    ( "-o",
      Arg.String (fun s -> output_file := Some s),
      " Remove summaries" );
    ( "-args",
      Arg.Expand Arg.read_arg,
      "<file> Read additional newline separated command line arguments \n\
      \      from <file>" );
    ( "-args0",
      Arg.Expand Arg.read_arg0,
      "<file> Read additional NUL separated command line arguments from \n\
      \      <file>" )
  ]

let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main () =
  Arg.parse_expand arg_list dump_obj arg_usage;
  exit 0

let _ = main ()
