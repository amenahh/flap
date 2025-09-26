type expectation =
  | Regex of string
  | Expect of { filename : string; }
  | Anything
  [@@deriving yojson]

(* The type of tests. *)
type t = {
    name : string;
    directory : string;
    description : string;
    source : string;
    arguments : string list;
    exitcode : int;
    expectation : expectation;
    tags : string list;
  } [@@deriving yojson]

val from_file : Path.t -> (t, PPrint.document) Stdlib.Result.t

module Set : Set.S with type elt = t
