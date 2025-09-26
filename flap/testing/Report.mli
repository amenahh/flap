module Render : sig
  type t

  val make :
    text:('a -> PPrint.document) ->
    html:('a -> Pure_html.node) ->
    'a -> t

  val of_string : string -> t

  val text_of : t -> PPrint.document

  val html_of : t -> Pure_html.node

  val nothing : t
end

module Error : sig
  type t

  val pp : t -> PPrint.document

  val make : ?details:Render.t -> string -> t

  val html_of : t -> Pure_html.node
end

module Status : sig
  type t =
    | Ok                          (** Test succeeded. *)
    | Ko of Error.t               (** Test failed. *)
    | Error of PPrint.document    (** Could not run test. *)

  val pp : t -> PPrint.document

  val html_of : t -> Pure_html.node
end

module TestResult : sig
  type t

  val compare : t -> t -> int

  val pp : ?verbose:bool -> t -> PPrint.document

  val make :
    name:string ->
    directory:string ->
    commandline:PPrint.document ->
    exitcode:int ->
    input:string ->
    output:string ->
    tags:string list ->
    Status.t ->
    t

  val wrap_error : ?name:string -> (t, PPrint.document) Result.t -> t

  val empty : t

  val html_of : t -> Pure_html.node
end

type t

val pp :
  ?verbose:bool ->
  ?only:[`Ok | `Ko | `All | `None] ->
  t -> PPrint.document

val make : TestResult.t list -> t

val html_of : ?mode:[`Full | `Article] -> t -> Pure_html.node

val all_good : t -> bool
