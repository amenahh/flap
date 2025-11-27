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
      | PVariable id ->
        if List.mem (Position.value id) vars then
          (* failwith "sayez chui deja la" *)
          let res = Identifier(Position.value id) in
          let s = "The variable " ^(string_of_binding res)^ " has already appeared in this pattern." in
          HopixTypes.type_error position s
          (* vars *)
        else
          (Position.value id)::vars
        (* failwith "PVAR" *)
      | PWildcard -> 
        vars
        (* failwith "WILDCARD" *)
      | PTypeAnnotation(pat,t) -> 
        check_pattern_linearity vars pat
        (* failwith "P TYPE ANO" *)
      | PLiteral(l) ->
        vars
        (* failwith "PLit" *)
      | PTaggedValue(c,typeList,p) -> 
        check_patternList_linearity vars p
        (* failwith "PTaggedVal" *)
      | PRecord(l,tl) -> 
        let pattern_list = List.map snd l in
        check_patternList_linearity vars pattern_list
        (* failwith "PRecord" *)
      | PTuple(l) -> 
        check_patternList_linearity vars l
        (* failwith "PTuple" *)
      | POr(l) -> 
        check_patternList_linearity vars l
        (* failwith "POR" *)
      | PAnd(l) -> 
        check_patternList_linearity vars l
        (* failwith "Pand" *)
      
        
and check_patternList_linearity identifier_list pattern_list =
  match pattern_list with
    | [] -> identifier_list
    | p::tl -> 
      let res = check_pattern_linearity identifier_list p in
        check_patternList_linearity res tl

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
    let _ = check_pattern_linearity [] pat in
    match p with
    | PVariable id -> 
      let var_scheme = monomorphic_type_scheme expected in
      bind_value (Position.value id) var_scheme env
    | PWildcard -> env
    | PTypeAnnotation(pat,t) -> 
      let annotated_type = internalize_ty env t in
      check_equal_types pos expected annotated_type;
      check_pattern env pat annotated_type
      (* failwith "P TYPE ANO" *)
    | PLiteral(l) -> 
      let res = synth_literal (Position.value l) in
      check_equal_types (Position.position l) expected res;
      env
      
    | PTaggedValue(c,typeList,p) -> 
      let res,new_env = synth_pattern env pat in
      check_equal_types pos expected res;
      new_env
      (* failwith "PTaggedVal" *)
    | PRecord(l,tl) -> failwith "PRecord"
    | PTuple(l) ->
      let res,new_env = synth_pattern env pat in
      check_equal_types pos expected res;
      new_env
       (* failwith "PTuple" *)
    | POr(l) -> 
      let res,new_env = synth_pattern env pat in
      check_equal_types pos expected res;
      new_env
      (* failwith "POR" *)
    | PAnd(l) ->
      let res,new_env = synth_pattern env pat in
      check_equal_types pos expected res;
      new_env
      (* failwith "Pand" *)

and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env Position.({ value = p; position = pos; } as pat) ->
  let _ = check_pattern_linearity [] pat in
  match p with
    | PVariable id -> 
      failwith "ft pas fr"
    | PWildcard -> 
      (* assert(false) *)
      failwith "WILDCARD"

    | PTypeAnnotation(pat,t) -> 
      let atyp = internalize_ty env t in
      let new_env = check_pattern env pat atyp in
      (atyp, new_env)
    | PLiteral(l) -> synth_literal (Position.value l) , env
    | PTaggedValue(kLocated, tyLocListOpt,pList) ->

      let aty_scheme =
      try
        lookup_type_scheme_of_constructor (Position.position kLocated) (Position.value kLocated) env
      with
      | HopixTypes.Unbound (pos_u, k) ->
          HopixTypes.type_error pos_u
            (Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding k))
      in
      let arity = match aty_scheme with Scheme(l, _) -> List.length l in
      let instantiated_type = 
        instantiate_with_type_list_option pos env aty_scheme arity tyLocListOpt 
      in
      let (arg_types, result_type) = destruct_function_type_maximally pos instantiated_type in
    
      let new_env = check_pattern_list env pList arg_types in
  
      (result_type, new_env)

    | PRecord(l,tl) -> failwith "PRecord"
    | PTuple(l) -> 
      let resType,new_env = synth_pTuple l env in
      ATyTuple(resType), new_env
      | POr(l) -> 
      begin
      match l with
      | b::tl -> 
        let attendu,new_env = synth_pattern env b in
        attendu,synth_pOr tl new_env attendu
      | [] -> failwith "POr"
      end
      (* failwith "POR" *)
    | PAnd(l) -> 
      begin
      match l with
      | b::tl -> 
        let attendu,new_env = synth_pattern env b in
        attendu,synth_pOr tl new_env attendu
      | [] -> failwith "Pand vide"
      end
      (* failwith "Pand" *)

and check_pattern_list env patterns expected_types =
  match patterns, expected_types with
  | [], [] -> env
  | p::ps, t::ts ->
      let newEnv = check_pattern env p t in
      check_pattern_list newEnv ps ts
  | _ ->
      failwith "Wrong number of arguments in pattern"

and synth_field l env attendu =
  match l with
  | [] -> env
  | p :: tl -> 
    let ptype, new_env = synth_pattern env p in
    check_equal_types (Position.position p) attendu ptype;
    synth_field tl new_env attendu


and synth_pOr l env  type_attendu =
  match l with
  | b::tl -> 
    let type_pattern,new_env = synth_pattern env b in
    check_equal_types (Position.position b) type_attendu type_pattern;
    synth_pOr tl new_env type_attendu
  | [] -> env     

(* and synth_precord l env =  *)

and instantiate_with_type_list_option pos env aty_scheme arity = function
  | Some tyList -> 
      let atys = List.map (internalize_ty env) tyList in
      instantiate_type_scheme aty_scheme atys
  | None -> 
      if arity <> 0 then
        failwith "aritéNone"
      else 
        instantiate_type_scheme aty_scheme []


and synth_pTuple l env =
  match l with
  | [] -> failwith "impossible"
  | [p] -> 
    let type_pattern, new_enw = synth_pattern env p in
    [type_pattern],new_enw
  | p::tl -> 
    let p_type , new_env = synth_pattern env p in
    let liste , environement_rec = synth_pTuple tl new_env in
    p_type::liste , environement_rec
    
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
     | Field (locExpr,locLabel,tyLocListOption) -> 
        let atyScheme = 
          try lookup_type_scheme_of_label (Position.position locLabel) (Position.value locLabel) env 
          with
          | HopixTypes.Unbound (pos_u, k ) ->
            HopixTypes.(type_error pos_u
                  Printf.(sprintf "Unbound %s." (string_of_binding k)))
        in
        let (tycon, arity, _) = 
        try
          lookup_type_constructor_of_label (Position.position locLabel) (Position.value locLabel) env
        with
        | HopixTypes.Unbound (pos_u, k) ->
            HopixTypes.type_error pos_u
              (Printf.sprintf "Unbound %s." (HopixTypes.string_of_binding k))
        in
        let tyField = instantiate_with_type_list_option pos env atyScheme arity tyLocListOption in
        let record_type, result_ty = destruct_function_type (Position.position locLabel) tyField in
        (check_expression env locExpr record_type);
        result_ty 
      

    | Tagged(kLocated, tyLocListOpt, exprLocList) -> 
        let listAty = List.map (fun exp -> synth_expression env exp) exprLocList in 
        let aty_scheme =
          try
            lookup_type_scheme_of_constructor (Position.position kLocated) (Position.value kLocated) env
          with
          | HopixTypes.Unbound (pos_u, k ) ->
            HopixTypes.(type_error pos_u
                  Printf.(sprintf
                            "Unbound %s."
                            (string_of_binding k)))
          in
        let arity = match aty_scheme with Scheme(l, _) -> List.length l in
        let instantiated_type = instantiate_with_type_list_option pos env aty_scheme arity tyLocListOpt in
        let (arg_types, result_type) = destruct_function_type_maximally pos instantiated_type in
        let rec check_args expected given = 
          (
          match expected , given with 
          |[] , [] -> ()
          | eh::et , gh::gt -> check_equal_types pos eh gh; check_args et gt
          | eh::et , [] -> 
            let partial_type = List.fold_right (fun t acc -> ATyArrow(t, acc)) (eh::et) result_type in
            check_equal_types pos result_type partial_type
          | [], gh -> failwith "julesss"
        ) in check_args arg_types listAty ;   result_type

        
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
   
    | Tuple locExprList -> 
      let atyList = List.map(fun locExpr -> (synth_expression env locExpr)) locExprList in
      ATyTuple atyList
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
      (* X *)
      let litType = synth_literal (Position.value litLoc) in
      check_equal_types pos expected litType;
    | Variable _ ->
       (* X *)
      begin
        try 
          let t = synth_expression env exp in
          check_equal_types pos expected t
        with 
          | Unbound (pos_u,k) -> 
            HopixTypes.(type_error pos_u
                  Printf.(sprintf
                            "Unbound %s."
                            (string_of_binding k)))

      end
    | Tagged(kLocated, tyLocListOpt, exprLocList) -> 
      (* X *)
      let givenAty = synth_expression env exp in
      check_equal_types pos givenAty expected
    
    | Record(l,typelist) -> 
      let recType = synth_expression env exp in
      check_equal_types pos expected recType
    
    | Field(expr,lab,typelist) ->
       (* X *)
      let f = synth_expression env exp in 
      check_equal_types pos expected f
    | Tuple _ -> 
      let given = synth_expression env exp in 
      check_equal_types pos expected given
    | Sequence(exprList) -> 
      (* X *)
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
       (* X *)
      let t1 = synth_expression env expr1 in
      let ty = destruct_reference_type (Position.position expr1) t1 in
      check_expression env expr2 ty;
      (* failwith "ASSIGN" *)
    | Read(expr) ->
       (* X *)
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
      (* X *)
      let t = synth_expression env exp in
      check_equal_types (Position.position expr) expected t

    | For(id,debutExpr,finExpr,body) ->
      let tfor = synth_expression env exp in
      check_equal_types (Position.position body) expected tfor;
    | TypeAnnotation(expr,t) -> 
      (* X *)
      let expr_aty =internalize_ty env t in
      check_equal_types pos expected expr_aty;
      check_expression env expr expr_aty

and synth_branch env branchList texpr =
  match branchList with
  | [] -> failwith "Pas exhaustif"
  | b::tl ->  
    let Branch(p,e) = Position.value b in
    let type_branch, new_env = synth_pattern env p in
    if type_branch = texpr then
      synth_expression new_env e
      (* failwith "" *)
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
      let exprAty = synth_expression env locExpr in 
      (* TODO *)
      let atyScheme = generalize_type env exprAty in
      (* let atyScheme = monomorphic_type_scheme exprAty in  *)
      bind_value (Position.value locId) atyScheme env

    )
  | RecFunctions _ -> failwith "rec function"



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
