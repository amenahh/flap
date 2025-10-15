%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}


%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA RARROW DIS LACC RACC AFFECTATION BACKSLASH
%token EQUAL LCHEVRON RCHEVRON DPOINTS BVERTICALE MOINS EXCLAMATION PLUS REF 
%token INTEROGATION BHORIZONTALE DIV STAR LT GT DRARROW EQ OR LTE DOT PVIRGULE
%token <string> VARIABLE
%token <string> CONST_ID
%token <string> IDENTIFICATEUR
// %token <string> ENTIER
%token <int64> ENTIER
%token <string> STRING
%token <char> CHAR 

%start<HopixAST.t> program
%%

program: ds = located(definition)* EOF
{  
   ds  
 
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


| vd = vdefinition {
  DefineValue vd
}


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


vdefinition:
| LET var_id=located(IDENTIFICATEUR) EQUAL exp=located(expr) {
  SimpleValue (Position.map (fun v -> Id v) var_id,None,exp)
}

| LET var_id=located(IDENTIFICATEUR) DPOINTS ts = located(typeScheme) EQUAL exp=located(expr){
  SimpleValue (Position.map (fun v -> Id v) var_id,Some ts,exp)
}

(* 
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
expr_atomic:
|i=located(ENTIER){
  Literal (Position.map (fun v -> LInt v) i )
}
|str = located(STRING){
  Literal (Position.map (fun v -> LString v) str)
}
|c=located(CHAR) {
  Literal   (Position.map (fun v -> LChar v) c )
}
|LPAR RPAR {
  Tuple([])
}
|LPAR el=exprlist RPAR {
  Tuple(el)
}
| LPAR e1=located(expr) DPOINTS t=located(htype) RPAR {
  TypeAnnotation (e1,t)
}
| MATCH LPAR e=located(expr) RPAR LACC b= branchList RACC {
  Case(e,b)
}
|DO LACC e1=located(expr) RACC UNTIL LPAR e2=located(expr) RPAR {
  While (e2,e1) (* Pas sur parce que techniquement until veux que While((!e2),e1) *)
}
| FOR var_id=located(IDENTIFICATEUR) FROM LPAR e1=located(expr) RPAR TO LPAR e2=located(expr) RPAR LACC e3=located(expr) RACC {
  For (Position.map (fun v -> Id v) var_id,e1,e2,e3)
}

app_chaine:
| e = located(expr_atomic) e1 = located(expr_atomic) {
  Apply(e,e1)
}

expr:
|ea = expr_atomic { ea }

| e=located(expr) DOT label_id=located(IDENTIFICATEUR) LCHEVRON t=typelist RCHEVRON {
  Field(e,Position.map (fun v -> LId v) label_id, Some t)
}
| e=located(expr) DOT label_id=located(IDENTIFICATEUR) {
  Field(e,Position.map (fun v -> LId v) label_id, None)
}


|LACC rl= recordlist RACC {
  Record(rl,None)
}
|LACC rl= recordlist RACC LCHEVRON tl=typelist RCHEVRON{
  Record(rl,Some tl)
}


| IF LPAR e1 = located(expr) RPAR THEN LACC e2 = located(expr) RACC ELSE LACC e3 = located(expr) RACC {
  IfThenElse(e1,e2,e3)
}
|WHILE LPAR e1=located(expr) RPAR LACC e2=located(expr) RACC {
  While (e1,e2)
}

|ac = app_chaine { ac }
| IF LPAR e1 = located(expr) RPAR THEN LACC e2 = located(expr) RACC {
  IfThenElse(e1,e2,(Position.with_poss $startpos $endpos (Tuple([]))))
}


|var_id=located(IDENTIFICATEUR) {
  Variable ((Position.map (fun v -> Id v) var_id),None)
}
|var_id=located(IDENTIFICATEUR) LCHEVRON tl = typelist RCHEVRON {
  Variable ((Position.map (fun v -> Id v) var_id),Some tl)
}
|constr_id=located(CONST_ID) {
  Tagged ((Position.map (fun v -> KId v) constr_id),None,[])
}
|constr_id=located(CONST_ID) LCHEVRON tl= typelist RCHEVRON {
  Tagged ((Position.map (fun v -> KId v) constr_id),Some tl,[])
}
|constr_id=located(CONST_ID) LPAR el = exprlist RPAR {
  Tagged ((Position.map (fun v -> KId v) constr_id),None,el)
}
|constr_id=located(CONST_ID) LCHEVRON tl= typelist RCHEVRON LPAR el = exprlist RPAR{
  Tagged ((Position.map (fun v -> KId v) constr_id),Some tl,el)
}
| e= located(expr) PVIRGULE e1 = located(expr) {
  Sequence( [e;e1] )
}
| v = vdefinition PVIRGULE e=located(expr) {
  Define(v,e)
}

| e = located(expr) binop e1 = located(expr){
  Apply(e,e1)
}

| e= located(expr) AFFECTATION e1 = located(expr) {
  Assign(e,e1)
}

| BACKSLASH p=located(pattern) RARROW e = located(expr) {
  Fun(FunctionDefinition(p,e))
}


| REF e= located(expr) {
  Ref(e)
}

| EXCLAMATION e=located(expr) {
  Read(e)
}


recordlist:
| label_id=located(IDENTIFICATEUR) EQUAL e=located(expr){
  [(Position.map (fun v -> LId v) label_id,e)]
}
| label_id=located(IDENTIFICATEUR) EQUAL e=located(expr) COMMA rl=recordlist{
  (Position.map (fun v -> LId v) label_id,e)::rl
}

branchList :
| BVERTICALE b = located(branche) {
  [b]
}
| BVERTICALE b = located(branche) BVERTICALE l=branchList {
  b::l
}
| b = located(branche) {
  [b]
}
| b = located(branche) BVERTICALE l=branchList {
  b::l
}

branche:
| p = located(pattern) RARROW e = located(expr) {
  Branch(p,e)
}

exprlist:
| e=located(expr) {
  [e]
}
| e=located(expr) COMMA el=exprlist {
  e::el
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
| t1=located(htype) tu = typeUplet{
  TyTuple (t1::tu)
}
| tv = VARIABLE {
  TyVar (TId tv)
}


typeUplet : 
| STAR t=located(htype) {
    [t]
}
| t=located(htype) STAR tu= typeUplet {
  (t):: tu
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
  (*PVariable(i)*)
  PVariable(Position.map (fun i -> Id i) i)
}
| BHORIZONTALE {
  PWildcard
}
| i = located(ENTIER) {
  PLiteral( Position.map (fun i -> LInt i) i)
}

| s = located (STRING) {
  PLiteral( Position.map (fun s -> LString s) s)
}

| c =located(CHAR) {
  PLiteral(   Position.map (fun c -> LChar c) c)

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
  PRecord(l,None)
}

| LPAR l = labelPatternList RPAR LCHEVRON t=typelist RCHEVRON {
  PRecord( l,Some t)
}


| c = located(CONST_ID) LCHEVRON t=typelist RCHEVRON LPAR l = listPattern RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, Some t,l)
}

| c = located(CONST_ID) {
  PTaggedValue(Position.map (fun v -> KId v) c, None,[])
}

| c = located(CONST_ID)  LPAR l = listPattern RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, None,l)
}

| c = located(CONST_ID) LCHEVRON t=typelist RCHEVRON  {
  PTaggedValue(Position.map (fun v -> KId v) c, Some t,[])
}


binop :
| PLUS {}
| MOINS {}
| STAR {}
| DIV {}
| OR {}
| AND  {}
| EQ {}
| LTE {}
| DRARROW {}
| LT {}
| GT {}



%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

