open! Int_replace_polymorphic_compare

type scope_item =
  | Sc_anonymous_function
  | Sc_value_definition
  | Sc_module_definition
  | Sc_class_definition
  | Sc_method_definition
  | Sc_partial_or_eta_wrapper
  | Sc_lazy

type scopes =
  | Empty
  | Cons of {
      item : scope_item;
      str : string;
      str_fun : string;
      name : string;
      prev : scopes;
      assume_zero_alloc : Assume_info.t;
    }

let str = function
  | Empty -> ""
  | Cons r -> r.str

let str_fun = function
  | Empty -> "(fun)"
  | Cons r -> r.str_fun

let cons scopes item str name ~assume_zero_alloc =
  Cons
    {
      item;
      str;
      str_fun = str ^ ".(fun)";
      name;
      prev = scopes;
      assume_zero_alloc;
    }

let empty_scopes = Empty

let add_parens_if_symbolic = function
  | "" -> ""
  | s -> (
      match s.[0] with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '_'
      | '0' .. '9' ->
          s
      | _ -> "(" ^ s ^ ")"
    )

let dot ?(sep = ".") ?no_parens scopes s =
  let s =
    match no_parens with
    | None -> add_parens_if_symbolic s
    | Some () -> s
  in
  match scopes with
  | Empty -> s
  | Cons { str; _ } -> str ^ sep ^ s

let enter_anonymous_function ~scopes ~assume_zero_alloc =
  let str = str_fun scopes in
  Cons
    {
      item = Sc_anonymous_function;
      str;
      str_fun = str;
      name = "";
      prev = scopes;
      assume_zero_alloc;
    }

let enter_value_definition ~scopes ~assume_zero_alloc id =
  cons scopes Sc_value_definition
    (dot scopes (Ident.name id))
    (Ident.name id) ~assume_zero_alloc

let enter_compilation_unit ~scopes cu =
  let name = Compilation_unit.name_as_string cu in
  cons scopes Sc_module_definition (dot scopes name) name
    ~assume_zero_alloc:Assume_info.none

let enter_module_definition ~scopes id =
  cons scopes Sc_module_definition
    (dot scopes (Ident.name id))
    (Ident.name id) ~assume_zero_alloc:Assume_info.none

let enter_class_definition ~scopes id =
  cons scopes Sc_class_definition
    (dot scopes (Ident.name id))
    (Ident.name id) ~assume_zero_alloc:Assume_info.none

let enter_method_definition ~scopes (s : Asttypes.label) =
  let str =
    match scopes with
    | Cons { item = Sc_class_definition; _ } -> dot ~sep:"#" scopes s
    | _ -> dot scopes s
  in
  cons scopes Sc_method_definition str s ~assume_zero_alloc:Assume_info.none

let enter_lazy ~scopes =
  cons scopes Sc_lazy (str scopes) "" ~assume_zero_alloc:Assume_info.none

let enter_partial_or_eta_wrapper ~scopes =
  cons scopes Sc_partial_or_eta_wrapper
    (dot ~no_parens:() scopes "(partial)")
    "" ~assume_zero_alloc:Assume_info.none

let join_assume_zero_alloc ~scopes ~assume_zero_alloc =
  match scopes with
  | Empty -> Empty
  | Cons r ->
      if Assume_info.equal r.assume_zero_alloc assume_zero_alloc then
        scopes
      else
        let assume_zero_alloc =
          Assume_info.join r.assume_zero_alloc assume_zero_alloc
        in
        Cons { r with assume_zero_alloc }

let get_assume_zero_alloc ~scopes =
  match scopes with
  | Empty -> Assume_info.none
  | Cons { assume_zero_alloc; _ } -> assume_zero_alloc

let string_of_scopes = function
  | Empty -> "<unknown>"
  | Cons { str; assume_zero_alloc; _ } ->
      str ^ Assume_info.to_string assume_zero_alloc

let string_of_scopes =
  let module StringSet = Set.Make (String) in
  let repr = ref StringSet.empty in
  fun scopes ->
    let res = string_of_scopes scopes in
    match StringSet.find_opt res !repr with
    | Some x -> x
    | None ->
        repr := StringSet.add res !repr;
        res

type t = Loc_unknown | Loc_known of { loc : Location.t; scopes : scopes }

let of_location ~scopes loc =
  if Location.is_none loc then
    Loc_unknown
  else
    Loc_known { loc; scopes }

let to_location = function
  | Loc_unknown -> Location.none
  | Loc_known { loc; _ } -> loc

let string_of_scoped_location = function
  | Loc_unknown -> "??"
  | Loc_known { loc = _; scopes } -> string_of_scopes scopes

let map_scopes f t =
  match t with
  | Loc_unknown -> Loc_unknown
  | Loc_known { loc; scopes } -> Loc_known { loc; scopes = f ~scopes }
