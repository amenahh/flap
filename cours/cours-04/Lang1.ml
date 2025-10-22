type t =
  | Var of Id.t
  | Int of int
  | Add of t * t
  | Let of t * bound1
  | Fun of bound1
  | App of t * t

and bound1 = { bound : Id.t; body : t; }

let rec pp ff = function
  | Var x ->
     Id.pp ff x
  | Int n ->
     Format.fprintf ff "%d" n
  | Add (e1, e2) ->
     Format.fprintf ff "add(@[@[%a@],@ @[%a@]@])"
       pp e1
       pp e2
  | Let (e1, e2) ->
     Format.fprintf ff "let(@[@[%a@],@ @[%a@]@])"
       pp e1
       pp_bound1 e2
  | Fun b ->
     Format.fprintf ff "fun(@[%a@])"
       pp_bound1 b
  | App (e1, e2) ->
     Format.fprintf ff "(@[@[%a@]@ @[%a@]@])"
       pp e1
       pp e2

and pp_bound1 ff { bound; body; } =
  Format.fprintf ff "@[%a.@,@[%a@]@]"
    Id.pp bound
    pp body

and pp_substitution ff xs =
  Fmt.list ~sep:Fmt.comma
    (fun ff (x, e) ->
      Format.fprintf ff "%a\\@[%a@]" Id.pp x pp e)
    ff xs

module Build = struct
  let fresh_bound1 mk =
    let x = Id.fresh () in { bound = x; body = mk (Var x); }
  let v x = Var x
  let l i = Int i
  let ( + ) e1 e2 = Add (e1, e2)
  let ( let* ) e1 mk_e2 =
    let bound = Id.fresh () in
    Let (e1, { bound; body = mk_e2 (Var bound); })
  let fun_ mk_body = Fun (fresh_bound1 mk_body)
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
  | Fun b ->
     Fun (subst_bound1 s b)
  | App (e1, e2) ->
     App (subst s e1, subst s e2)

and subst_bound1 s { bound = x; body; } =
  let x' = Id.fresh () in
  { bound = x'; body = subst (Id.extend x (Var x') s) body; }

and compose s1 s2 =
  match s1 with
  | [] -> s2
  | (x, t) :: s1 -> (x, subst s2 t) :: compose s1 s2

let rec equal e e' =
  match e, e' with
  | Var x, Var x' ->
     x = x'
  | Int i, Int i' ->
     i = i'
  | (Add (e1, e2), Add (e1', e2')) | (App (e1, e2), App (e1', e2')) ->
     equal e1 e1' && equal e2 e2'
  | Let (e1, e2), Let (e1', e2') ->
     equal e1 e1' && equal_bound1 e2 e2'
  | Fun b, Fun b' ->
     equal_bound1 b b'
  | _ ->
     false

and equal_bound1 b1 b2 =
  let y = Id.fresh () in
  equal
    (subst Id.(singleton b1.bound (Var y)) b1.body)
    (subst Id.(singleton b2.bound (Var y)) b2.body)

and equal_subst sub1 sub2 = Id.equal_subst equal sub1 sub2

type value = VInt of int | VFun of bound1 * env and env = (Id.t * value) list

let rec eval : env -> t -> value =
  fun env e ->
  match e with
  | Var x -> Id.assoc x env
  | Int k -> VInt k
  | Add (e1, e2) -> add_values (eval env e1) (eval env e2)
  | Let (e, b) -> eval_bound1 env b (eval env e)
  | Fun b -> VFun (b, env)
  | App (e1, e2) -> app_values (eval env e1) (eval env e2)

and eval_bound1 env { bound; body; } v =
  eval ((bound, v) :: env) body

and add_values v1 v2 =
  match v1, v2 with VInt k1, VInt k2 -> VInt (k1 + k2)
                  | _ -> failwith "ill-typed"

and app_values v_fun v_arg =
  match v_fun with VFun (b, env) -> eval_bound1 env b v_arg
                 | _ -> failwith "ill-typed"

let rec pp_value ff = function
  | VInt k ->
     Format.fprintf ff "%d" k
  | VFun (b, env) ->
     Format.fprintf ff "@[%a{%a}@]"
       pp_bound1 b
       (Id.pp_subst pp_value) env

let%expect_test "e1" =
  Id.reset ();
  let print_eval e =
    Format.printf "@[eval @[%a@]@ = %a@]@."
      pp e
      pp_value (eval Id.empty e)
  in
  let open Build in
  print_eval (let* x = l 1 in x);
  [%expect {| eval let(1, _x1._x1) = 1 |}];
  print_eval (let* x = l 1 in x);
  [%expect {| eval let(1, _x2._x2) = 1 |}];
  print_eval (let* x = l 1 in let* k = l 2 in x + k);
  [%expect {| eval let(1, _x3.let(2, _x4.add(_x3, _x4))) = 3 |}];
  print_eval (let* x = l 1 in let* k = l 2 in x + k);
  [%expect {| eval let(1, _x5.let(2, _x6.add(_x5, _x6))) = 3 |}]
