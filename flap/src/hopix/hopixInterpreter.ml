open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)
type 'e gvalue =
  | VInt       of Mint.t
  | VChar      of char
  | VString    of string
  | VUnit
  | VTagged    of constructor * 'e gvalue list
  | VTuple     of 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VLocation  of Memory.location
  | VClosure   of 'e * pattern located * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue -> 'e gvalue)

(** Two values for booleans. *)
let ptrue  = VTagged (KId "True", [])
let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let fail = None
let ret x = Some x
let value_as_int      = function VInt x -> ret x | _ -> fail
let value_as_char     = function VChar c -> ret c | _ -> fail
let value_as_string   = function VString s -> ret s | _ -> fail
let value_as_tagged   = function VTagged (k, vs) -> ret (k, vs) | _ -> fail
let value_as_record   = function VRecord fs -> ret fs | _ -> fail
let value_as_location = function VLocation l -> ret l | _ -> fail
let value_as_closure  = function VClosure (e, p, b) -> ret (e, p, b) | _ -> fail
let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail
let value_as_bool = function
  | VTagged (KId "True", []) -> true
  | VTagged (KId "False", []) -> false
  | _ -> assert false

(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)
type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (** To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Mint.to_string x
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VLocation a ->
          print_array_value d (Memory.dereference m a)
        | VTagged (KId k, []) ->
          k
        | VTagged (KId k, vs) ->
          k ^ print_tuple d vs
        | VTuple (vs) ->
           print_tuple d vs
        | VRecord fs ->
           "{"
           ^ String.concat ", " (
                 List.map (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v
           ) fs) ^ "}"
        | VClosure _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
    and print_tuple d vs =
      "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
    and print_array_value d block =
      let r = Memory.read block in
      let n = Mint.to_int (Memory.size block) in
      "[ " ^ String.concat ", " (
                 List.(map (fun i -> print_value (d + 1) (r (Mint.of_int i)))
                         (ExtStd.List.range 0 (n - 1))
               )) ^ " ]"
  in
  print_value 0 v

let print_values m vs =
  String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)
type value = Environment.t gvalue

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f
: value
= VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    let error m v =
      Printf.eprintf
        "Invalid arguments for `%s': %s\n"
        name (print_value m v);
      assert false (* By typing. *)
    in
    VPrimitive (name, fun m -> function
      | VInt x ->
         VPrimitive (name, fun m -> function
         | VInt y -> out (op x y)
         | v -> error m v)
      | v -> error m v)
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Mint.(
    [ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [ ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun _ x -> VPrimitive (name, fun _ y ->
        out (op (value_as_bool x) (value_as_bool y))))
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops =
    [ ("`||`", ( || )); ("`&&`", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m v ->
      output_string stdout (print_value m v);
      flush stdout;
      VUnit
    )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun _ -> function
      | VInt x -> print (Mint.to_string x)
      | _ -> assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun _ -> function
      | VString x -> print x
      | _ -> assert false (* By typing. *)
    )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         ptrue
  |> bind' "false"        pfalse
  |> bind' "nothing"      VUnit

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)

and definition runtime d = 
  let def = Position.value d in
  match def with
  | DefineValue vd -> 
    { runtime with environment =  valDefinition runtime vd }
    
  | _ -> runtime


and valDefinition runtime vd =
  match vd with 
  | SimpleValue (id,_,e) -> 
    let valID = Position.value id in
    let valeur =  expression' runtime.environment runtime.memory e in 
   Environment.bind  runtime.environment valID valeur
  | RecFunctions (l) ->  
    failwith "RecFuctions"

    (* List.fold_left valPolymorphic runtime l *)

(* and valPolymorphic runtime element =
  match element with
  | (id,_,FunctionDefinition(pattern,expr)) ->  
    (*
    idée mais pas sûre faire appel à la fct pr pattern puis utiliser une monade pr passer à expr ??
    *)
    failwith "failure in valPolymorphic" *)

and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)

   and expression _ environment memory e =
    match e with
      | Literal x -> valLitteral(x)
      | Variable(id,_) -> 
        let valId = Position.value id in
        let posId = Position.position id in
          Environment.lookup  posId valId environment 
      | Apply(e1,e2) ->
        let e1Value = expression' environment memory e1 in
        let e2Value = expression' environment memory e2 in
        valApply e1Value e2Value memory
      | IfThenElse(e1,e2,e3) -> 
        let e1Value = expression' environment memory e1 in
        valIfThenElse e1Value e2 e3 environment memory
      | Sequence(l) -> sequence_val l environment memory
      | Tagged(c,_,l) ->
        let cval = Position.value c in
        let liste = tuple_val l environment memory in
        VTagged(cval,liste)
      | Tuple(l) -> 
        let liste = tuple_val l environment memory in
        VTuple(liste)
      | Record(l,_) -> 
        let liste = record_val l environment memory in
        VRecord(liste)
      
      | Field(e,li,_) -> 
        field_val e li environment memory
      
      | While(e1,e2) -> while_val e1 e2 environment memory

      | For(x,e1,e2,e3) -> 
        let v1 = expression' environment memory e1 in
        let v2 = expression' environment memory e2 in
        for_val x v1 v2 e3 environment memory
        (* failwith "For" *)

      
      | Define(vd,e) ->
        let r_actuelle = { environment; memory} in
        let  r_env = valDefinition r_actuelle vd in
        expression' r_env r_actuelle.memory e
      (* revoir la mémoire *)
             
      | Ref(exprLoc) ->
        let valeur = expression' environment memory exprLoc in
        (* TODO savoir la taille à allouer  *)
        let alloue = Memory.allocate memory Mint.one valeur  in
        VLocation(alloue) 

      
      | Assign(eLoc1,eLoc2) ->
        let e1 = expression' environment memory eLoc1 in
        let e2 = expression' environment memory eLoc2 in
        assign_val e1 e2 memory
        (* TODO retrouver le bloc ds lequel on ft le write *)
      
      
      | Read(exprLoc) -> 
        let valeur = expression' environment memory exprLoc in
        read_val memory valeur
      
      | Case(exprLoc,branchlist) -> 
        let valExpr = expression' environment memory exprLoc in 
        let valMatch = List.find_map (fun x -> val_branch valExpr environment memory (Position.value x) ) branchlist 
        in (match valMatch with 
        |Some v -> v
        |None -> failwith "Case"
        )

      |Fun(_) -> failwith "Fun expression"

      | TypeAnnotation(_,_) -> failwith "Ano"

and val_pattern environment valExpression pattern = 
  match pattern,valExpression with 
  | PTaggedValue (cons,_,plist),VTagged(cval,listval) -> 
    if (Position.value cons) = cval then 
      list_val_pattern environment listval plist
    else None 
  | PTypeAnnotation(pl,_),_ -> val_pattern  environment valExpression (Position.value pl)
  | PRecord (pl,_),VRecord(vl) -> record_val_pattern environment vl pl
  | PVariable(id),_ ->
    let valId = Position.value id in Some (Environment.bind  environment valId valExpression )
  | PWildcard,_ -> Some environment
  | PLiteral(l), vlit -> 
    if (valLitteral l) = vlit then Some environment
    else None 
  | PTuple(l),VTuple(vl) -> 
    list_val_pattern  environment vl l 
  | POr(pl),_ -> pattern_or pl valExpression environment
  | PAnd(pl),_ -> pattern_and pl valExpression environment
  | _,_ -> None

and val_v v =
  match v with
  | VInt(i) -> i
  | _ -> failwith "Pas un Mint"

and val_branch valExpression environment memory = function
  | Branch(locPat,locExpr) ->  let v = val_pattern environment valExpression (Position.value locPat) in 
  match v with 
  | Some newEnv -> Some (expression' newEnv memory locExpr)
  | None -> None
  

and assign_val e1 e2 mem =
  match e1 with
  | VLocation(addr) -> 
    let b = Memory.dereference mem addr in
    Memory.write b Mint.zero e2;
    VUnit
  | _ -> failwith "Pas une référence" 

and read_val  memory valeur =
    match  valeur with
    | VLocation(addr) -> 
      let b = Memory.dereference memory addr in
      Memory.read b Mint.zero
      (* pq c zero ? j'aurais mis Mint.one *)
    | _ -> failwith "C'est pas une référence"

(* TODO pas tester encore le while *)
and while_val e1 e2 env m =
  let v = expression' env m e1 in
  if value_as_bool v then 
    let r = expression' env m e2 in 
    while_val e1 e2 env m
  else VUnit

 and for_val id from toval expr env m =
  let pos = Position.position id in
  let valID = Position.value id in
  if (val_v from) <= (val_v toval) then 
    let valE = expression' env m expr in 
    Environment.update pos valID env (VInt (Mint.add (getInt from ) Mint.one));
    for_val id (VInt(Mint.add (getInt from ) Mint.one)) toval expr env m   
  else 
    VUnit 
    

and getInt value = 
      match value with
     |VInt mint -> mint 
     | _ -> failwith "Error not VInt in for" 

and field_val e li environment memory =
  (* expression' environment memory e *)
  let value = expression' environment memory e in 
  match value  with 
  |VRecord(list) -> 
    trouve list (Position.value li)
  |_ -> failwith "Field"  
  
and trouve l lab =
  match  l with
  |  [] -> failwith "pas trouve"
  | (v,t)::tl-> if v = lab then t else trouve tl lab

and record_val l env memory = 
  match l with
  | [] -> []
  | (lab,expr)::tl -> 
    let valE = expression' env memory expr in
    let valLab = Position.value lab in
    (valLab,valE):: record_val tl env memory

and tuple_val l env memory =
  match l with
  | [] -> []
  | h::tl -> 
    let valH = expression' env memory h in
    valH::tuple_val tl env memory

and list_val_pattern env valExpression lpat= 
  match lpat,valExpression with
  | [],[] -> Some env
  | hp::tlp, hv::tlv  -> 
    let p = Position.value hp in 
    let valEnv = val_pattern env hv p in
    (
    match valEnv with
    | Some e ->  list_val_pattern e tlv tlp 
    | None -> None
    )
  | _,_ -> None

and record_val_pattern env valExpression lpat= 
  match lpat,valExpression with
  | [],[] -> Some env
  | (locLabP,exprp)::tlp, (labv,exprv)::tlv  -> 
    let labP  = Position.value locLabP in 
    if labP = labv then (
      let p = Position.value exprp in
      let valEnv = val_pattern env exprv p in
      (
      match valEnv with
      | Some e ->  record_val_pattern e tlv tlp
      | None -> None
      )
    )
    else None 
  | _,_ -> None  
and pattern_or l v env =
    match l with
    | [] -> failwith "pas possible"
    | [p] ->
      val_pattern env  v (Position.value p) 
    | p::tl -> 
      let nv_env = val_pattern env  v (Position.value p)  in
      match nv_env with
      | Some _ -> nv_env
      | None -> pattern_or tl v env  

and pattern_and l v env =
    match l with
    | [] -> failwith "pas possible"
    | [p] ->
      val_pattern env  v (Position.value p) 
    | p::tl -> 
      let nv_env = val_pattern env  v (Position.value p)  in
      match nv_env with
      | Some e -> pattern_and tl v e
      | None -> None

and sequence_val l env memory =
  match  l with
  | [] -> 
    VUnit
    (* TODO à revoir *)
  | [h] -> expression' env memory h 
  | h::tl -> 
    let v = expression' env memory h in 
    sequence_val tl env memory 


and valIfThenElse v e2 e3 env memory =
  if value_as_bool v then expression' env memory e2
  else expression' env memory e3

and valApply e1 e2 m = 
  match e1 with
  | VPrimitive(_,f) -> f m e2
  | _ -> failwith "ouch"

and valLitteral l =
  let lit = Position.value l in
  match lit with
  | LInt i -> VInt(i)
  | LString(s) -> VString(s)
  | LChar(c) -> VChar(c)

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

(** This function displays a difference between two runtimes. *)
let print_observable (_ : runtime) observation =
  Environment.print observation.new_memory observation.new_environment

