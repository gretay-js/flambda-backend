(* TEST

flags = "-w +A-70"
compile_only = "true"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output

* no-flambda
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** check-ocamlopt.byte-output

* flambda
compiler_reference = "${test_source_directory}/w59.flambda.reference"
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** check-ocamlopt.byte-output

*)

(* Check that the warning 59 (assignment to immutable value) does not
   trigger on those examples *)
let a = Lazy.force (lazy "a")
let b = Lazy.force (lazy 1)
let c = Lazy.force (lazy 3.14)
let d = Lazy.force (lazy 'a')
let e = Lazy.force (lazy (fun x -> x+1))
let rec f (x:int) : int = g x and g x = f x
let h = Lazy.force (lazy f)
let i = Lazy.force (lazy g)
let j = Lazy.force (lazy 1L)
let k = Lazy.force (lazy (1,2))
let l = Lazy.force (lazy [|3.14|])
let m = Lazy.force (lazy (Sys.opaque_identity 3.14))
let n = Lazy.force (lazy None)

(* Check that obviously wrong code is reported *)
let o = (1,2)
let p = fun x -> x
let q = 3.14
let r = 1

(* %obj_set_field is OK here for Flambda 2 because we never use
   it on an array.  We can't use Obj.field since that function
   contains a [Sys.opaque_identity]. *)
external set_field : Obj.t -> int -> Obj.t -> unit = "%obj_set_field"

let () =
  set_field (Obj.repr o) 0 (Obj.repr 3);
  set_field (Obj.repr p) 0 (Obj.repr 3);
  set_field (Obj.repr q) 0 (Obj.repr 3);
  set_field (Obj.repr r) 0 (Obj.repr 3)

let set v =
  set_field (Obj.repr v) 0 (Obj.repr 3)
  [@@inline]

let () =
  set o

(* Sys.opaque_identity hides all information and shouldn't warn *)

let opaque = Sys.opaque_identity (1,2)
let set_opaque =
  set_field
    (Obj.repr opaque)
    0
    (Obj.repr 3)
