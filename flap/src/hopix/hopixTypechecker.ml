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
    let tv = List.map (fun x -> Position.value x ) ts in 
    let newEnv = bind_type_variables pos env tv in
    let aty = internalize_ty env ty in 
    ((Scheme (tv,aty)),newEnv)


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
    match p with
    | PVariable id -> 
      let type_id = lookup_type_scheme_of_identifier (Position.position id) (Position.value id) env in
      let aty_id = instantiate_type_scheme type_id [] in
      check_equal_types pos expected aty_id;
      env
    | PWildcard -> env
    | PTypeAnnotation(pat,t) -> failwith "P TYPE ANO"
    | PLiteral(l) -> 
      let res = synth_literal (Position.value l) in
      check_equal_types (Position.position l) expected res;
      env
      
    | PTaggedValue(c,typeList,p) -> failwith "PTaggedVal"
    | PRecord(l,tl) -> failwith "PRecord"
    | PTuple(l) -> failwith "PTuple"
    | POr(l) -> failwith "POR"
    | PAnd(l) -> failwith "Pand"

and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env Position.{ value = p; position = pos; } ->
  match p with
    | PVariable id -> 
      let type_id = lookup_type_scheme_of_identifier (Position.position id) (Position.value id) env in
      let aty_id = instantiate_type_scheme type_id [] in
      aty_id , env;
    | PWildcard -> failwith "WILDCARD"
    | PTypeAnnotation(pat,t) -> 
      let atyp = internalize_ty env t in
      let ptype,new_env = synth_pattern env pat in
      check_equal_types pos ptype atyp; 
      ptype,new_env
    | PLiteral(l) -> synth_literal (Position.value l) , env
    
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
    | Record(labelExprList,typelist_opt) -> 
      let lab,_ = List.hd labelExprList in
      let cons,arity , labelList = lookup_type_constructor_of_label (Position.position lab) (Position.value lab) env in
        begin
          match typelist_opt with
          | Some l -> 
            let atylist = List.map (internalize_ty env) l in
               record_check  labelExprList labelList atylist env ;
            ATyCon(cons,atylist)
          | None -> 
              record_check labelExprList labelList [] env;
            ATyCon(cons,[])
        end

    | Field(expr,lab,typelist) -> failwith "FIELD"
    | Tuple(exprList) -> failwith "OK"
    | Sequence(exprList) -> 
      (* synth_sequence exprList env *)
      failwith "SEQUENCE"
    | Define(v,expr) -> 
      let newEnv = check_value_definition env v in
      synth_expression newEnv expr
      (* failwith "DEFINE" *)
    | Fun(f) -> failwith "FUN"
    | Apply(expr1,expr2) ->
      let v1 = synth_expression env expr1 in
      let t1,t2 = destruct_function_type (Position.position expr1) v1 in
      check_expression env expr2 t1;
      t2
    | Ref(expr) -> href (synth_expression env expr)
      (* failwith "REF" *)
    | Assign(expr1,expr2) -> 
      let t1 = synth_expression env expr1 in
      let ty = destruct_reference_type (Position.position expr1) t1 in
      check_expression env expr2 ty;
      ty
      (* let t1 = synth_expression env expr1 in
      check_expression env expr2 t1;
      t1 *)
      (* failwith "ASSIGN" *)
    | Read(expr) -> 
      synth_expression env expr
      (* failwith "READ" *)
    | Case(e,b) -> failwith "CASE"
    | IfThenElse(e1,e2,e3) -> 
      (* () *)
      failwith "if then else"
    | While(condition,expr) -> 
      let tcond = synth_expression env condition in
      check_equal_types (Position.position condition) hbool tcond;
      let _ = synth_expression env expr in
      hunit
    | For(id,debutExpr,finExpr,body) ->
      let tdeb = synth_expression env debutExpr in
      let tfin = synth_expression env finExpr in
      check_equal_types (Position.position finExpr) tdeb tfin;
      let tbody = 
        synth_expression env body ;
      in
      hunit
    | TypeAnnotation(expr,t) ->
        let expr_aty =internalize_ty env t in
        check_expression env expr expr_aty;
        expr_aty
       (* failwith "TYPE ANNO" *)


       
(* vérifier que tt les labels apparaissent exactement une fois avc les bons types *)
(* je dois synth les expressions du label et chercher le type qui est dans 
l'env pr le comparer à lui
*)
and record_check labelExprList labelList atylist env =
  match labelExprList with
  | [] -> ()
  | (label, expr) :: tl ->
    if List.mem (Position.value label) labelList then
      let tyExpr = synth_expression env expr in
      let expected_type_scheme = lookup_type_scheme_of_label (Position.position label) (Position.value label) env in 
      let attendu =  instantiate_type_scheme expected_type_scheme atylist in
      let _,final = destruct_function_type (Position.position label) attendu in
        check_equal_types (Position.position label) final tyExpr ;  
        record_check tl labelList atylist env
      else 
        failwith "C PAS CA"

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
      begin
        try 
          let t = synth_expression env exp in
          check_equal_types pos expected t
        with 
          | Unbound (pos,b) -> type_error pos (string_of_binding b)
      end
    | Tagged(cloc,typelist,exprList) -> failwith "TAGGED"
    
    | Record(l,typelist) -> 
      let recType = synth_expression env exp in
      check_equal_types pos expected recType
    
    | Field(expr,lab,typelist) -> failwith "FIELD"
    | Tuple(exprList) -> failwith "TUPLE"
    | Sequence(exprList) -> 
      let _ = synth_sequence exprList env in
      ()
    | Define(v,expr) -> 
      let newEnv = check_value_definition env v in
      check_expression newEnv expr expected
      (* failwith "DEFINE" *)
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
    | Case(e,b) -> 
      let texpr = synth_expression env e in
      let type_branch = synth_branch env b texpr in
      check_equal_types pos expected type_branch
      (* failwith "CASE" *)
    | IfThenElse(e1,e2,e3) ->
      let t1 = synth_expression env e1 in
      check_equal_types (Position.position e1) hbool t1;
      let t2 = synth_expression env e2 in
      let t3 = synth_expression env e3 in
      check_equal_types (Position.position e3) t2 t3
      (* failwith "if then else" *)
    | While(condition,expr) -> 
      let t = synth_expression env exp in
      check_equal_types (Position.position expr) expected t

      | For(id,debutExpr,finExpr,body) ->
      let tfor = synth_expression env exp in
      check_equal_types (Position.position body) expected tfor;
    | TypeAnnotation(expr,t) ->
      let expr_aty =internalize_ty env t in
      check_expression env expr expr_aty;

and synth_branch env branchList texpr =
  match branchList with
  | [] -> failwith "Pas exhaustif"
  | b::tl ->  
    let Branch(p,e) = Position.value b in
    let type_branch, new_env = synth_pattern env p in
    if type_branch = texpr then
      failwith ""
    else
      synth_branch env tl texpr

and check_value_definition :
      HopixTypes.typing_environment ->
      HopixAST.value_definition ->
      HopixTypes.typing_environment
  = fun env def -> match def with
  | SimpleValue(locId,opTyScheme,locExpr) -> 
    (match opTyScheme with
    
    |Some locTyScheme -> 
      (
      let tySchemeEnv = check_type_scheme env (Position.position locTyScheme) (Position.value locTyScheme) 
      in match tySchemeEnv with
      |(atyScheme,newEnv) -> 
        match atyScheme with
        |(Scheme (_,aty)) ->
          (*Pas sur si on met le newEnv ou le env*)
          check_expression newEnv locExpr aty;
          bind_value (Position.value locId) atyScheme env
      )
    |None -> 
      let exrpAty = synth_expression env locExpr in 
      let atyScheme = generalize_type env exrpAty in 
      bind_value (Position.value locId) atyScheme env

    )
  | RecFunctions (list_function_def_poly_def) -> failwith "rec function"



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
