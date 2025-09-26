module Configuration : sig
  type t =
    {
      command : string;
    } [@@deriving yojson]

  val command_line_options : t ref -> (Arg.key * Arg.spec * Arg.doc) list
end

val run : Configuration.t -> Test.t -> Report.TestResult.t
