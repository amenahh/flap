(** {1 Internal Types} *)

(** This module defines an (OCaml) type of (Hopix) types as manipulated by the
    typechecker. Such {i internal} types differ from the source-level {i
    external} types defined in {! HopixAST} in that they are not annotated with
    source-level positions.

    In a more realistic compiler, the distance between internal and external
    types would be greater. *)

open HopixAST

(** Internal types. *)
type aty =
  | ATyVar   of type_variable
  | ATyCon   of type_constructor * aty list
  | ATyTuple of aty list
  | ATyArrow of aty * aty

(** Internal type schemes. *)
type aty_scheme = Scheme of type_variable list * aty

(** {2 Error management} *)

val type_error : Position.t -> string -> 'a

(** {2 Utility functions} *)

val string_of_aty : aty -> string

val monomorphic_type_scheme : aty -> aty_scheme

val instantiate_type_scheme : aty_scheme -> aty list -> aty

(** {3 Type construction and destruction} *)

val hunit : aty
val hint : aty
val hbool : aty
val hstring : aty
val hchar : aty
val hprod : aty list -> aty
val href : aty -> aty

(** The type of destructuring functions, which receive a type and destruct it
    into its constituents. Such functions fail by raising an exception if the
    type is not of the expected shape, e.g., [destruct_function_type (ATyVar x)]
    always raise an exception. *)
type 'res destruction_fun = Position.t -> aty -> 'res

val destruct_function_type : (aty * aty) destruction_fun

val destruct_function_type_maximally : (aty list * aty) destruction_fun

val destruct_product_type : aty list destruction_fun

val destruct_constructed_type : (type_constructor * aty list) destruction_fun

val destruct_reference_type : aty destruction_fun

(** {2 Typing Contexts} *)

type typing_environment

val initial_typing_environment : unit -> typing_environment

val string_of_typing_environment : typing_environment -> string

val bind_type_variable :
  Position.t -> typing_environment -> type_variable -> typing_environment

val bind_type_variables :
  Position.t -> typing_environment -> type_variable list -> typing_environment

(** [internalize_ty env ety] internalizes the external type [ety]. It signals an
    error when [ety] is not well-formed in [env]. *)
val internalize_ty : typing_environment -> ty Position.located -> aty

type binding =
  | Identifier of identifier
  | TypeConstructor of type_constructor
  | Constructor of constructor
  | Label of label

val string_of_binding : binding -> string

exception Unbound of Position.position * binding

exception InvalidInstantiation of { expected : int; given : int; }

type 'key type_scheme_lookup_fun =
  Position.t -> 'key -> typing_environment -> aty_scheme

val lookup_type_scheme_of_constructor : constructor type_scheme_lookup_fun

val lookup_type_scheme_of_label : label type_scheme_lookup_fun

val lookup_type_scheme_of_identifier : identifier type_scheme_lookup_fun

val lookup_type_constructor_of_label
    : Position.t -> label -> typing_environment ->
      type_constructor * int * label list

val lookup_fields_of_type_constructor
    : Position.t -> type_constructor -> typing_environment -> label list

val bind_value
    : identifier -> aty_scheme -> typing_environment -> typing_environment

val bind_type_definition
    : type_constructor -> type_variable list -> type_definition
      -> typing_environment -> typing_environment

val string_of_typing_environment : typing_environment -> string

val generalize_type : typing_environment -> aty -> aty_scheme
