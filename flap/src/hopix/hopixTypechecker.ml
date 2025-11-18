(** This module implements a bidirectional type checker for Hopix. *)

open HopixAST
open HopixTypes
(** Error messages *)

let invalid_instantiation pos given expected =
  HopixTypes.type_error pos (
      Printf.sprintf
        "Invalid number of types in instantiation: \
         %d given while %d were expected." given expected
    )

let check_equal_types pos ~expected ~given =
  if expected <> given
  then
    HopixTypes.(type_error pos
                  Printf.(sprintf
                            "Type mismatch.\nExpected:\n  %s\nGiven:\n  %s"
                            (string_of_aty expected)
                            (string_of_aty given)))

(** Linearity-checking code for patterns *)

(* MOI *)
let rec check_pattern_linearity
        : identifier list -> pattern Position.located -> identifier list
  = fun vars Position.{ value; position; } ->
    match value with
      | PVariable id -> failwith "PVAR"
      | PWildcard -> failwith "WILDCARD"
      | PTypeAnnotation(pat,t) -> failwith "P TYPE ANO"
      | PLiteral(l) -> failwith "PLit"
      | PTaggedValue(c,typeList,p) -> failwith "PTaggedVal"
      | PRecord(l,tl) -> failwith "PRecord"
      | PTuple(l) -> failwith "PTuple"
      | POr(l) -> failwith "POR"
      | PAnd(l) -> failwith "Pand"
      
(** Type-checking code *)
(* renvoie un env *)
let check_type_scheme :
      HopixTypes.typing_environment ->
      Position.t ->
      HopixAST.type_scheme ->
      HopixTypes.aty_scheme * HopixTypes.typing_environment
  = fun env pos (ForallTy (ts, ty)) ->
    failwith "AAA"

  (* MOI *)
let synth_literal : HopixAST.literal -> HopixTypes.aty =
  fun l ->
    match  l with
    | LInt _ -> HopixTypes.hint
    | LString _ -> HopixTypes.hstring
    | LChar _ -> HopixTypes.hchar
    
let rec check_pattern :
          HopixTypes.typing_environment ->
          HopixAST.pattern Position.located ->
          HopixTypes.aty ->
          HopixTypes.typing_environment
  = fun env Position.({ value = p; position = pos; } as pat) expected ->
    failwith "todo check_pattern"

  (* MOI *)
and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env Position.{ value = p; position = pos; } ->
  match p with
    | PVariable id -> failwith "PVAR"
    | PWildcard -> failwith "WILDCARD"
    | PTypeAnnotation(pat,t) -> failwith "P TYPE ANO"
    | PLiteral(l) -> failwith "PLit"
    | PTaggedValue(c,typeList,p) -> failwith "PTaggedVal"
    | PRecord(l,tl) -> failwith "PRecord"
    | PTuple(l) -> failwith "PTuple"
    | POr(l) -> failwith "POR"
    | PAnd(l) -> failwith "Pand"

    (*     
x avec t_barre comme type scheme se synthéthise en t' ou on remplece alpha par t_barre

SI t_barre est un type bien formé 

ET que la variable x avec comme type pourtout alpha_bare t' est dans l'environement *)

let rec synth_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty
  = fun env Position.{ value = e; position = pos; } ->
    match e with
    | Literal litLoc -> synth_literal (Position.value litLoc)
    | Variable(idLoc,lopt) ->
      begin
      match lopt with
      | Some l -> 
        let scheme = List.map (internalize_ty env) l in 
        let atyScheme = lookup_type_scheme_of_identifier (Position.position idLoc) (Position.value idLoc ) env  in
       instantiate_type_scheme atyScheme scheme
      | None ->
        let atyScheme = lookup_type_scheme_of_identifier (Position.position idLoc) (Position.value idLoc ) env  in
      instantiate_type_scheme atyScheme []
      end
    | Tagged(cloc,typelist,exprList) -> failwith "TAGGED"
    | Record(l,typelist) -> failwith "RECORD"
    | Field(expr,lab,typelist) -> failwith "FIELD"
    | Tuple(exprList) -> failwith "TUPLE"
    | Sequence(exprList) -> 
      (* synth_sequence exprList env *)
      failwith "SEQUENCE"
    | Define(v,expr) -> failwith "DEFINE"
    | Fun(f) -> failwith "FUN"
    | Apply(expr1,expr2) ->
      let v1 = synth_expression env expr1 in
      let t1,t2 = destruct_function_type (Position.position expr1) v1 in
      check_expression env expr2 t1;
      t2
    | Ref(expr) -> href (synth_expression env expr)
      (* failwith "REF" *)
    | Assign(expr1,expr2) -> 
      (* let t1 = synth_expression env expr1 in
      check_expression env expr2 t1;
      t1 *)
      failwith "ASSIGN"
    | Read(expr) -> 
      synth_expression env expr
      (* failwith "READ" *)
    | Case(e,b) -> failwith "CASE"
    | IfThenElse(e1,e2,e3) -> failwith "if then else"
    | While(condition,expr) -> failwith "while"
    | For(id,debutExpr,finExpr,body) ->
      hunit
       (* failwith "FOR" *)
    | TypeAnnotation(expr,t) ->
        let expr_aty =internalize_ty env t in
        check_expression env expr expr_aty;
        expr_aty
       (* failwith "TYPE ANNO" *)

and synth_sequence exprList env =
  match exprList with
  | [] -> failwith "c pas possible"
  | [e] -> synth_expression env e
  | expr::l -> 
    check_expression env expr hunit;
    synth_sequence l env

  (* MOI *)
and check_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty ->
      unit
  = fun env (Position.{ value = e; position = pos; } as exp) expected ->
    match e with 
    | Literal litLoc -> 
      let litType = synth_literal (Position.value litLoc) in
      check_equal_types pos expected litType;
    | Variable _ ->
      let t = synth_expression env exp in
      check_equal_types pos expected t
    | Tagged(cloc,typelist,exprList) -> failwith "TAGGED"
    | Record(l,typelist) -> failwith "RECORD"
    | Field(expr,lab,typelist) -> failwith "FIELD"
    | Tuple(exprList) -> failwith "TUPLE"
    | Sequence(exprList) -> 
      let _ = synth_sequence exprList env in
      (* failwith "SEQUENCE" *)
      ()
    | Define(v,expr) -> failwith "DEFINE"
    | Fun(f) -> failwith "FUN"
    | Apply _ -> 
      let t = synth_expression env exp in 
      check_equal_types pos expected t;
    | Ref _ -> 
      let t = synth_expression env exp in
      check_equal_types (Position.position exp) expected t
      (* failwith "REF" *)
    | Assign(expr1,expr2) -> 
      let t1 = synth_expression env expr1 in
      let ty = destruct_reference_type (Position.position expr1) t1 in
      check_expression env expr2 ty;
      (* failwith "ASSIGN" *)
    | Read(expr) -> 
      let ty = synth_expression env expr in
      let t = destruct_reference_type (Position.position expr) ty in
      check_equal_types pos expected t
      (* failwith "READ" *)
    | Case(e,b) -> failwith "CASE"
    | IfThenElse(e1,e2,e3) -> failwith "if then else"
    | While(condition,expr) -> failwith "while"
    | For(id,debutExpr,finExpr,body) -> failwith "FOR"
    | TypeAnnotation(expr,t) ->
      let expr_aty =internalize_ty env t in
      check_expression env expr expr_aty;
        
      (* () *)
       (* failwith "TYPE ANNO" *)
  
  

and check_value_definition :
      HopixTypes.typing_environment ->
      HopixAST.value_definition ->
      HopixTypes.typing_environment
  = fun env def ->
    failwith "TODO"
  (* synthèse *)
let check_definition env = function
  | DefineValue vdef ->
     check_value_definition env vdef

  | DefineType (t, ts, tdef) ->
     let ts = List.map Position.value ts in
     HopixTypes.bind_type_definition (Position.value t) ts tdef env

  | DeclareExtern (x, tys) ->
     let tys, _ = Position.located_pos (check_type_scheme env) tys in
     HopixTypes.bind_value (Position.value x) tys env

let typecheck env program = (* TODO à completer *)
  List.fold_left
    (fun env d -> Position.located (check_definition env) d)
    env program

type typing_environment = HopixTypes.typing_environment

let initial_typing_environment = HopixTypes.initial_typing_environment

let print_typing_environment = HopixTypes.string_of_typing_environment
