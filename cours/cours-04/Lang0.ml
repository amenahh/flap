type t =
  | Var of Id.t
  | Int of int
  | Add of t * t
  | Let of t * bound1

and bound1 = { bound : Id.t; body : t; }

let rec pp ff = function
  | Var x ->
     Id.pp ff x
  | Int n ->
     Format.fprintf ff "%d" n
  | Add (e1, e2) ->
     Format.fprintf ff "@[@[%a@]@ + @[%a@]@]"
       pp e1
       pp e2
  | Let (e1, e2) ->
     Format.fprintf ff "let(@[@[%a@],@ @[%a@]@])"
       pp e1
       pp_bound1 e2

and pp_bound1 ff { bound; body; } =
  Format.fprintf ff "@[%a.@,@[%a@]@]"
    Id.pp bound
    pp body

let pp_substitution ff xs =
  Fmt.list ~sep:Fmt.comma
    (fun ff (x, e) ->
      Format.fprintf ff "%a\\@[%a@]" Id.pp x pp e)
    ff xs

module Build = struct
  let v x = Var x
  let l i = Int i
  let ( + ) e1 e2 = Add (e1, e2)
  let ( let* ) e1 mk_e2 =
    let bound = Id.fresh () in
    Let (e1, { bound; body = mk_e2 (Var bound); })
end

let rec subst s = function
  | Var x ->
     begin try Id.assoc x s with Not_found -> Var x end
  | Int k ->
     Int k
  | Add (e1, e2) ->
     Add (subst s e1, subst s e2)
  | Let (e1, e2) ->
     Let (subst s e1, subst_bound1 s e2)

and subst_bound1 s { bound = x; body; } =
  let x' = Id.fresh () in
  { bound = x'; body = subst (Id.extend x (Var x') s) body; }

let%expect_test "subst" =
  Id.reset ();
  let print_subst e x e' =
    Format.printf "@[(@[%a@])[%a\\@[%a@]]@ = @[%a@]@]@."
      pp e
      Id.pp x
      pp e'
      pp (subst [(x, e')] e)
  in
  let open Build in
  print_subst (l 1 + v "x") "x" (v "y");
  [%expect {| (1 + x)[x\y] = 1 + y |}];
  print_subst (Let (l 2, { bound = "y"; body = l 1 + v "x" + v "y"; }))
    "x" (v "y");
  [%expect {| (let(2, y.1 + x + y))[x\y] = let(2, _x1.1 + y + _x1) |}];
  ();;

let rec equal e e' =
  match e, e' with
  | Var x, Var x' ->
     x = x'
  | Int i, Int i' ->
     i = i'
  | Add (e1, e2), Add (e1', e2') ->
     equal e1 e1' && equal e2 e2'
  | Let (e1, e2), Let (e1', e2') ->
     equal e1 e1' && equal_bound1 e2 e2'
  | _ ->
     false

and equal_bound1 { bound = x; body = e; } { bound = x'; body = e'; } =
  let y = Id.fresh () in
  equal
    (subst Id.(singleton x (Var y)) e)
    (subst Id.(singleton x' (Var y)) e')

and equal_subst sub1 sub2 = Id.equal_subst equal sub1 sub2

let%expect_test "equal" =
  Id.reset ();
  let print_equal e1 e2 =
    Format.printf "@[@[%a@]@ = @[%a@]: %b@]@."
      pp e1
      pp e2
      (equal e1 e2)
  in
  let open Build in
  print_equal (let* x = l 1 in x) (let* y = l 1 in y);
  [%expect {| let(1, _x2._x2) = let(1, _x1._x1): true |}];
  print_equal (let* x = l 1 in x) (let* y = l 2 in y);
  [%expect {| let(1, _x5._x5) = let(2, _x4._x4): false |}];
  print_equal
    (let* x = l 1 in let* k = l 2 in x + k)
    (let* y = l 1 in let* k = l 2 in y + k);
  [%expect {|
    let(1, _x8.let(2, _x9._x8 + _x9)) = let(1, _x6.let(2, _x7._x6 + _x7)): true |}];
  print_equal
    (let* x = l 1 in let* k = l 2 in x + k)
    (let* y = l 1 in let* k = l 2 in k + y);
  [%expect {|
    let(1, _x16.let(2, _x17._x16 + _x17))
    = let(1, _x14.let(2, _x15._x15 + _x14)): false |}];;

let rec free_vars : t -> Id.Set.t = function
  | Var x -> Id.Set.singleton x
  | Int _ -> Id.Set.empty
  | Add (e1, e2) -> Id.Set.union (free_vars e1) (free_vars e2)
  | Let (e1, e2) -> Id.Set.union (free_vars e1) (free_vars_bound1 e2)

and free_vars_bound1 { bound; body; } =
  Id.Set.remove bound (free_vars body)

let%expect_test "free_vars" =
  Id.reset ();
  let print_free_vars e =
    Format.printf "@[free_vars (@[%a@])@ = {@[%a@]}@]@."
      pp e
      Fmt.(list ~sep:comma Id.pp) (free_vars e |> Id.Set.to_seq |> List.of_seq)
  in
  let open Build in
  print_free_vars (l 1 + l 2);
  [%expect {| free_vars (1 + 2) = {} |}];
  print_free_vars (let* x = l 12 in l 1 + x + v "y");
  [%expect {| free_vars (let(12, _x1.1 + _x1 + y)) = {y} |}];
  print_free_vars (let* x = l 12 in let* y = v "z" in l 1 + x + y);
  [%expect {| free_vars (let(12, _x2.let(z, _x3.1 + _x2 + _x3))) = {z} |}];
  ()

let close_with { bound; body; } e = subst Id.(singleton bound e) body

let rec eval : t -> int =
  function
  | Var _ -> failwith "open term"
  | Int k -> k
  | Add (e1, e2) -> eval e1 + eval e2
  | Let (e, b) -> eval (close_with b e)

(* Tests fixtures *)

let e1, e2, e3, e4 =
  Id.reset ();
  let open Build in
  (l 1 + l 1),
  (let* x = l 1 + l 1 in x),
  (let* x = l 1 + l 1 in l 40 + x),
  (let* x = l 1 + l 1 in let* y = x + l 1 in let* x = l 3 in x + y)
