open HopixAST

module LabelSet = Set.Make (struct
  type t = label
  let compare = compare
end)

module TypeVariableSet = Set.Make (struct
  type t = type_variable
  let compare = compare
end)

let fresh_identifier =
  let count = ref (-1) in
  fun () -> incr count; Id ("id" ^ string_of_int !count)
