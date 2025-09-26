(** This module implements a type checker for Hopix. *)

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let typecheck tenv _ =
  tenv

let print_typing_environment =
  HopixTypes.string_of_typing_environment
