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
  failwith "Students! This is your job!"

(** Type-checking code *)

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


let synth_literal : HopixAST.literal -> HopixTypes.aty =
  fun l ->
  failwith "Students! This is your job!"

let rec check_pattern :
          HopixTypes.typing_environment ->
          HopixAST.pattern Position.located ->
          HopixTypes.aty ->
          HopixTypes.typing_environment
  = fun env Position.({ value = p; position = pos; } as pat) expected ->
  failwith "Students! This is your job!"

and synth_pattern :
      HopixTypes.typing_environment ->
      HopixAST.pattern Position.located ->
      HopixTypes.aty * HopixTypes.typing_environment
  = fun env Position.{ value = p; position = pos; } ->
  failwith "Students! This is your job!"

let rec synth_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty
  = fun env Position.{ value = e; position = pos; } ->
  failwith "Students! This is your job!"

and check_expression :
      HopixTypes.typing_environment ->
      HopixAST.expression Position.located ->
      HopixTypes.aty ->
      unit
  = fun env (Position.{ value = e; position = pos; } as exp) expected ->
  failwith "Students! This is your job!"

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

let typecheck env program =
  List.fold_left
    (fun env d -> Position.located (check_definition env) d)
    env program

type typing_environment = HopixTypes.typing_environment

let initial_typing_environment = HopixTypes.initial_typing_environment

let print_typing_environment = HopixTypes.string_of_typing_environment
