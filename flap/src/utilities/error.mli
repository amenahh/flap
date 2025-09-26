(** This module provides a uniform way of reporting (located) error messages. *)

(** [exit_on_error ()] forces the program to stop if an error is encountered.
    (This is the default behavior.) *)
val exit_on_error: unit -> unit

(** [resume_on_error ()] makes the program throw the exception {!Error}
    if an error is encountered. *)
val resume_on_error: unit -> unit

(** The value of [print_locs] controls whether locations are printed by the
    functions of this module. It is set to [true] by default. *)
val print_locs : bool ref

exception Error of Position.t list * string

(** [print_error positions msg] formats an error message. *)
val print_error : Position.t list -> string -> string

(** [error k p msg] prints [msg] with [k] as a message prefix and stops
    the program. *)
val error : string -> Position.t -> string -> 'a

(** [error2 k p1 p2 msg] prints two positions instead of one. *)
val error2 : string -> Position.t -> Position.t -> string -> 'a

(** [errorN k ps msg] prints several positions. *)
val errorN : string -> Position.t list -> string -> 'a

(** [global_error k msg] prints [msg] with [k] as a message prefix and stops
    the program. *)
val global_error : string -> string -> 'a
