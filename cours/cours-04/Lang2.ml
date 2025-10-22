(** {2 Abstract Syntax Trees} *)

type t = Var of Id.t           (* occurrence "x", "y", etc. *)
       | Int of int            (* constante litérale "42", etc. *)
       | Add of t * t          (* somme "e1 + e2" *)
       | Bool of bool          (* booléen litéral "true", "false" *)
       | If of t * t * t       (* conditionnelle *)
       | Let of t * bound1     (* déf. locale "let e1 be x in e2" *)
       | Fun of bound1         (* fonction anonyme "fun x -> e" *)
       | App of t * t          (* application "e1 e2" *)
       | Ref of t              (* allocation "ref e" *)
       | Read of t             (* déréférencement "!e" *)
       | Asgn of t * t         (* assignation "e1 := e2" *)
       | Unit                  (* 0-uplet *)
       | Seq of t * t          (* séquence "e1; e2" *)

and bound1 = { bound : Id.t; body : t; }

(** {2 Evaluation} *)

type env = (Id.t * value) list

and value =
  | VInt of int
  | VBool of bool
  | VFun of bound1 * env
  | VRef of Memory.Address.t
  | VUnit

type mem = value Memory.t

type 'a stateful = mem -> 'a * mem
let return : 'a -> 'a stateful = fun v m -> v, m
let ( >>= ) xm f m = let x, m = xm m in f x m
let ( let+ ) = ( >>= )

let rec eval : env -> t -> value stateful =
  fun env t ->
  match t with
  | Var x ->
     return (List.assoc x env)
  | Int n ->
     return (VInt n)
  | Add (e1, e2) ->
     let+ v1 = eval env e1 in
     let+ v2 = eval env e2 in
     add_values v1 v2
  | Bool b ->
     return (VBool b)
  | If (e1, e2, e3) ->
     let+ v1 = eval env e1 in
     if_value v1 env e2 e3
  | Let (e, b) ->
     let+ v = eval env e in
     eval_bound1 env b v
  | Fun b ->
     return (VFun (b, env))
  | App (e1, e2) ->
     let+ v1 = eval env e1 in
     let+ v2 = eval env e2 in
     app_values v1 v2
  | Ref e ->
     let+ v = eval env e in
     let+ a = Memory.alloc v in
     return (VRef a)
  | Read e ->
     let+ v = eval env e in
     read_value v
  | Asgn (e1, e2) ->
     let+ v1 = eval env e1 in
     let+ v2 = eval env e2 in
     asgn_values v1 v2
  | Unit ->
     return VUnit
  | Seq (e1, e2) ->
     let+ v1 = eval env e1 in
     seq_value v1 env e2

and eval_bound1 env { bound; body; } v =
  eval ((bound, v) :: env) body

and add_values v1 v2 =
  match v1, v2 with
  | VInt n1, VInt n2 -> return (VInt (n1 + n2))
  | _ -> failwith "ill-typed"

and if_value v1 env e2 e3 =
  match v1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> failwith "ill-typed"

and app_values v1 v2 =
  match v1 with
  | VFun (b, env) -> eval_bound1 env b v2
  | _ -> failwith "ill-typed"

and read_value v =
  match v with VRef a -> fun m -> Memory.read a m, m
             | _ -> failwith "ill-typed"

and asgn_values v1 v2 =
  match v1 with VRef a -> fun m -> VUnit, Memory.asgn a v2 m
              | _ -> failwith "ill-typed"

and seq_value v1 env e2 =
  match v1 with VUnit -> eval env e2 | _ -> failwith "ill-typed"

(** {2 Pretty-printing} *)

let rec pp ff =
  let pp_maybe_paren ?(space = true) prefix pp ff e =
    match e with
    | Var _ | Int _ | Bool _ | Unit ->
       Format.fprintf ff "@[%s%a@[%a@]@]"
         prefix
         Fmt.(if space then sp else nop) ()
         pp e
     | _ ->
        Format.fprintf ff "@[%s(@[%a@])@]"
          prefix
          pp e
  in
  function
  | Var x ->
     Id.pp ff x
  | Int n ->
     Format.fprintf ff "%d" n
  | Add (e1, e2) ->
     Format.fprintf ff "(@[@[%a@] +@ @[%a@]@])"
       pp e1
       pp e2
  | Bool b ->
     Format.fprintf ff "%b" b
  | If (e1, e2, e3) ->
     Format.fprintf ff "if(@[@[%a@],@ @[%a@],@ @[%a@]@])"
       pp e1
       pp e2
       pp e3
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
  | Ref e ->
     pp_maybe_paren "ref" pp ff e
  | Read e ->
     pp_maybe_paren ~space:false "!" pp ff e
  | Asgn (e1, e2) ->
     Format.fprintf ff "(@[@[%a@]@ := @[%a@]@])"
       pp e1
       pp e2
  | Unit ->
     Format.fprintf ff "()"
  | Seq (e1, e2) ->
     Format.fprintf ff "(@[@[%a@];@ @[%a@]@])"
       pp e1
       pp e2

and pp_bound1 ff { bound; body; } =
  Format.fprintf ff "@[%a.@,@[%a@]@]"
    Id.pp bound
    pp body

let rec pp_value ff = function
  | VInt n ->
     Format.fprintf ff "%d" n
  | VBool b ->
     Format.fprintf ff "%b" b
  | VRef a ->
     Format.fprintf ff "ref(%a)" Memory.Address.pp a
  | VUnit ->
     Format.fprintf ff "()"
  | VFun (b, env) ->
     Format.fprintf ff "@[<v 2>fun(@[%a@])@,[@[%a@]]@]"
       pp_bound1 b
       pp_env env

and pp_env ff env =
  let pp_binding ff (x, v) =
    Format.fprintf ff "@[<hv 2>%a@ = @[%a@]@]"
      Id.pp x
      pp_value v
  in
  Fmt.(list ~sep:comma pp_binding) ff env

(** {2 AST Building Helpers} *)

module Build = struct
  let fresh_bound1 mk =
    let x = Id.fresh () in { bound = x; body = mk (Var x); }

  let v x = Var x
  let i n = Int n
  let ( + ) e1 e2 = Add (e1, e2)
  let b b = Bool b
  let if_ e1 e2 e3 = If (e1, e2, e3)
  let ( let* ) e1 mk_e2 = Let (e1, fresh_bound1 mk_e2)
  let fun_ mk_body = Fun (fresh_bound1 mk_body)
  let app f args = List.fold_left (fun f arg -> App (f, arg)) f args
  let ref_ e = Ref e
  let ( ! ) e = Read e
  let ( := ) e1 e2 = Asgn (e1, e2)
  let s = function
    | [] -> Unit
    | e :: es -> List.fold_left (fun e1 e2 -> Seq (e1, e2)) e es
end

(** {2 Tests} *)

let print_eval e =
  let v, _ = eval [] e Memory.empty in
  Format.printf "@[<hv 2>@[%a@]@ => @[%a@]@]@."
    pp e
    pp_value v

let%expect_test "test" =
  Id.reset ();
  let open Build in
  print_eval (i 1 + i 2);
  [%expect {| (1 + 2) => 3 |}];
  print_eval (let* x = ref_ (i 1) in
              s [x := i 2; !x] + !x);
  [%expect {| let(ref 1, _x1.(((_x1 := 2); !_x1) + !_x1)) => 4 |}];
  print_eval (let* x = ref_ (i 1) in
              !x + s [x := i 2; !x]);
  [%expect {| let(ref 1, _x2.(!_x2 + ((_x2 := 2); !_x2))) => 3 |}]
