open AST

let string_of_exp e =
  let rec aux = function
    | Var x ->
       x
    | Int x ->
       string_of_int x
    | Add (e1, e2) ->
       Printf.sprintf "(%s + %s)" (aux e1) (aux e2)
    | Fun b ->
       Printf.sprintf "(fun %s -> %s)"
         (String.concat " " b.bound)
         (aux b.body)
    | App (f, a) ->
       "(" ^ String.concat " " (List.map aux (f::a)) ^ ")"
  in
  aux e
