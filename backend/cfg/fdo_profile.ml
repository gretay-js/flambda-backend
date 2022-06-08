(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2022 Jane Street Group LLC                                  *
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
[@@@ocaml.warning "+a-30-40-41-42"]

include Fdo_profile_intf.S

(* Marshal and unmarshal a compilation unit in Cfg format *)

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save filename t =
  let ch = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string ch Config.fdo_magic_number;
      output_value ch t;
      (* Compute digest of the contents and append it to the file. *)
      flush ch;
      let crc = Digest.file filename in
      Digest.output ch crc)
    ~always:(fun () -> close_out ch)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let restore filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
      let magic = Config.fdo_magic_number in
      let buffer = really_input_string ic (String.length magic) in
      if String.equal buffer magic
      then
        try
          let t = (input_value ic : t) in
          let crc = Digest.input ic in
          t, crc
        with
        | End_of_file | Failure _ -> raise (Error (Corrupted filename))
        | Error e -> raise (Error e)
      else if String.sub buffer 0 9 = String.sub magic 0 9
      then raise (Error (Wrong_version filename))
      else raise (Error (Wrong_format filename)))
    ~always:(fun () -> close_in ic)

(* Error report *)

open Format

let report_error ppf = function
  | Wrong_format filename ->
    fprintf ppf "Expected Fdo_profile format. Incompatible file %a"
      Location.print_filename filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is not compatible with this version of OCaml"
      Location.print_filename filename
  | Corrupted filename ->
    fprintf ppf "Corrupted format@ %a" Location.print_filename filename
  | Marshal_failed filename ->
    fprintf ppf "Failed to marshal Fdo_profile to file@ %a"
      Location.print_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
