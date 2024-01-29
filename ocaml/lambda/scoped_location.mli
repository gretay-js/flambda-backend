type scope_item = private
  | Sc_anonymous_function
  | Sc_value_definition
  | Sc_module_definition
  | Sc_class_definition
  | Sc_method_definition
  | Sc_partial_or_eta_wrapper
  | Sc_lazy

type scopes = private
  | Empty
  | Cons of {
      item : scope_item;
      str : string;
      str_fun : string;
      name : string;
      prev : scopes;
      assume_zero_alloc : Assume_info.t;
    }

val string_of_scopes : scopes -> string
val empty_scopes : scopes

val enter_anonymous_function :
  scopes:scopes -> assume_zero_alloc:Assume_info.t -> scopes

val enter_value_definition :
  scopes:scopes -> assume_zero_alloc:Assume_info.t -> Ident.t -> scopes

val enter_compilation_unit : scopes:scopes -> Compilation_unit.t -> scopes
val enter_module_definition : scopes:scopes -> Ident.t -> scopes
val enter_class_definition : scopes:scopes -> Ident.t -> scopes
val enter_method_definition : scopes:scopes -> Asttypes.label -> scopes
val enter_lazy : scopes:scopes -> scopes
val enter_partial_or_eta_wrapper : scopes:scopes -> scopes

val join_assume_zero_alloc :
  scopes:scopes -> assume_zero_alloc:Assume_info.t -> scopes

val get_assume_zero_alloc : scopes:scopes -> Assume_info.t

type t = Loc_unknown | Loc_known of { loc : Location.t; scopes : scopes }

val of_location : scopes:scopes -> Location.t -> t
val to_location : t -> Location.t
val string_of_scoped_location : t -> string
val map_scopes : (scopes:scopes -> scopes) -> t -> t
