%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}


%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA RARROW DIS LACC RACC
%token EQUAL LCHEVRON RCHEVRON DPOINTS BVERTICALE MOINS EXCLAMATION PLUS REF 
%token INTEROGATION BHORIZONTALE DIV STAR LT GT DRARROW EQ OR LTE
%token <string> VARIABLE
%token <string> CONST_ID
%token <string> IDENTIFICATEUR
%token <string> ENTIER
%token <string> STRING
%token <char> CHAR 

%start<HopixAST.t> program
%%

program: ds = definition* EOF
{  
   []

}

definition:
(* type type_con [ < type_variable { , type_variable } > ] [ = tdefinition ] *)
| TYPE type_con=located(IDENTIFICATEUR) {
  DefineType (Position.map (fun v -> TCon v) type_con,[],Abstract)
} 
| TYPE type_con=located(IDENTIFICATEUR) EQUAL t=tdefinition {
    DefineType (Position.map (fun v -> TCon v) type_con,[],t)
}
| TYPE type_con=located(IDENTIFICATEUR) LCHEVRON tv = located(VARIABLE) RCHEVRON {
  DefineType (Position.map (fun v -> TCon v) type_con,[Position.map (fun v -> TId v) tv],Abstract) 
}
| TYPE type_con= located(IDENTIFICATEUR) LCHEVRON tvl = typevarlist  RCHEVRON EQUAL t=tdefinition{
  DefineType (Position.map (fun v -> TCon v) type_con,tvl,t)
}
| TYPE type_con=located(IDENTIFICATEUR) LCHEVRON tvl = typevarlist RCHEVRON {
  DefineType (Position.map (fun v -> TCon v) type_con,tvl,Abstract)
}
(*extern var_id : type_scheme*)
| EXTERN var_id =located(IDENTIFICATEUR) ts = located(typeScheme) {
  DeclareExtern(Position.map (fun v -> Id v) var_id,ts)
}
(*
| vd = vdefinition {
  DefineValue vd
}
*)
tdefinition:
| BVERTICALE? constr_id = located(CONST_ID) {
  DefineSumType [(Position.map (fun v -> KId v) constr_id,[])]
}
| BVERTICALE? constr_id = located(CONST_ID) LPAR tl=typelist RPAR {
  DefineSumType [(Position.map (fun v -> KId v) constr_id,tl)]
}
| BVERTICALE? constr_id = located(CONST_ID) LPAR tl=typelist RPAR td = tdefinitioniter {
  DefineSumType ((Position.map (fun v -> KId v) constr_id,tl)::td)
}
| BVERTICALE? constr_id = located(CONST_ID) td = tdefinitioniter {
  DefineSumType ((Position.map (fun v -> KId v) constr_id,[])::td)
}
| LACC label_id=located(IDENTIFICATEUR) DPOINTS t = located(htype)  RACC{
  DefineRecordType [(Position.map (fun v -> LId v) label_id,t)]
}
| LACC label_id=located(IDENTIFICATEUR) DPOINTS t = located(htype) COMMA tl= tylablist RACC{
  DefineRecordType ((Position.map (fun v -> LId v) label_id,t)::tl)
}

tylablist:
|  label_id=located(IDENTIFICATEUR) DPOINTS t = located(htype) {
  [(Position.map (fun v -> LId v) label_id,t)]
}
| label_id=located(IDENTIFICATEUR) DPOINTS t = located(htype) COMMA tl= tylablist{
  (Position.map (fun v -> LId v) label_id, t)::tl
}



(*J'ai fais ca car si la liste de tdef a un element le | est optionnel mais c'est pas le cas pour les tdef suivants*)
tdefinitioniter:
| BVERTICALE constr_id = located(CONST_ID) {
  [(Position.map (fun v -> KId v) constr_id,[])]
}
| BVERTICALE constr_id = located(CONST_ID) LPAR tl=typelist RPAR {
  [(Position.map (fun v -> KId v) constr_id,tl)]
}
| BVERTICALE constr_id = located(CONST_ID) LPAR tl=typelist RPAR td = tdefinitioniter {
  (Position.map (fun v -> KId v) constr_id,tl)::td
}
| BVERTICALE constr_id = located(CONST_ID) td = tdefinitioniter {
  (Position.map (fun v -> KId v) constr_id,[])::td
}
 (*
vdefinition:
| LET var_id=IDENTIFICATEUR EQUAL exp=expr {
  SimpleValue (located var_id,None,located exp)
}

| LET var_id=IDENTIFICATEUR DPOINTS ts = typeScheme EQUAL exp=expr{
  SimpleValue (located var_id,Some (located ts),located exp)
}

 
| FUN fd = fundef {
  RecFunctions [(located ,)]
}

| FUN fd = fundef lf = listfun {
  RecFunctions (fd) 
}

listfun: 
| AND fd {

}
| AND fd lf = listfun {

}
*)
expr:
|c=CHAR {
  LChar c 
}

htype : 
|type_con=IDENTIFICATEUR {
  TyCon (TCon type_con,[])
}
|type_con=IDENTIFICATEUR LCHEVRON tl=typelist RCHEVRON {
  TyCon (TCon type_con,tl)
}
| t1=located(htype) RARROW t2=located(htype) {
  TyArrow (t1,t2)
}
| LPAR t=htype RPAR {
  t
}
| t1=located(htype) STAR tu = typeUplet{
  TyTuple (t1::tu)
}
| tv = VARIABLE {
  TyVar (TId tv)
}


typeUplet : 
| t=located(htype) STAR tu= typeUplet {
  (t):: tu
}
| t=located(htype){
    [t]
}

typelist : 
| t=located(htype) COMMA tl=typelist {
  t::tl
}
|t=located(htype){
  [t]
}

typevarlist : 
| t=located(VARIABLE) COMMA tl=typevarlist {
  (Position.map (fun v -> TId v) t)::tl
}
|t=located(VARIABLE){
  [Position.map (fun v -> TId v) t]
}



typeScheme :
| LCROCHET l=typevarlist RCROCHET t=located(htype) {
  ForallTy(l,t)
} 
| t=located(htype) {
  ForallTy([],t)
}



labelPatternList :
| id = located(IDENTIFICATEUR) EQUAL p = located(pattern) {
  [Position.map (fun v -> LId v) id, p]
}

| id = located(IDENTIFICATEUR) EQUAL p = located(pattern) COMMA l=labelPatternList {
  (Position.map (fun v -> LId v) id ,p) :: l
}


listPattern :
| p=located(pattern) {
  [p]
}
| p=located(pattern) COMMA l=listPattern{
  p :: l
}

pattern :
| i = located(IDENTIFICATEUR) {
  PVariable(i)
}
| b = BHORIZONTALE {
  PWildcard()
}
| i = located(ENTIER) {
  PLiteral( LInt(i))
}

| LPAR RPAR {PTuple([])}

| LPAR p=listPattern RPAR {
  PTuple(p)
}

| p=located(pattern) DPOINTS t = located(htype){
  PTypeAnnotation(p,t)
}

| p=located(pattern) BVERTICALE pp=located(pattern) {
  POr(p ::[pp])
}
| p = located(pattern) DIS pp=located(pattern) {
  PAnd(p::[pp])
}

| LPAR l = labelPatternList RPAR {
  PRecord(l,p)
}

| LPAR l = labelPatternList RPAR LCHEVRON t=typelist RCHEVRON {
  PRecord( l,p)
}


| c = located(CONST_ID) LCHEVRON t=typelist RCHEVRON LPAR l = listPattern RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, t,l)
}

| c = located(CONST_ID) {
  PTaggedValue(Position.map (fun v -> KId v) c, [],[])
}

| c = located(CONST_ID)  LPAR l = listPattern RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, [],l)
}

| c = located(CONST_ID) LCHEVRON t=typelist RCHEVRON  {
  PTaggedValue(Position.map (fun v -> KId v) c, t,[])
}

//TODO char
//RODOString


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
