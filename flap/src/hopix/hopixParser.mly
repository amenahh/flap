%{ (* -*- tuareg -*- *)

  open HopixAST
  (*
  open Position
   open Mint
   *)
%}

%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA RARROW DIS LACC RACC AFFECTATION BACKSLASH
%token EQUAL LCHEVRON RCHEVRON DPOINTS BVERTICALE MOINS EXCLAMATION PLUS REF PREFIX
%token BHORIZONTALE DIV STAR LT GT DRARROW EQ OR LTE DOT PVIRGULE VDEFPREC TEST 

%token <string> VARIABLE
%token <string> CONST_ID
%token <string> IDENTIFICATEUR
%token <Mint.t> ENTIER

%token <string> STRING
%token <char> CHAR 

%right TEST
%right RARROW 
%right AFFECTATION

%right PVIRGULE 
%nonassoc VDEFPREC
%left DPOINTS
%left PLUS MOINS
%left STAR DIV
%left AND OR
%left GT LT LTE EQ DRARROW

%right PREFIX


%left DIS BVERTICALE



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
| TYPE type_con= located(IDENTIFICATEUR) LCHEVRON tvl =separated_nonempty_list(COMMA,var)  RCHEVRON EQUAL t=tdefinition{
  DefineType (Position.map (fun v -> TCon v) type_con,tvl,t)
}
| TYPE type_con=located(IDENTIFICATEUR) LCHEVRON tvl =separated_nonempty_list(COMMA,var) RCHEVRON {
  DefineType (Position.map (fun v -> TCon v) type_con,tvl,Abstract)
}
(*extern var_id : type_scheme*)
| EXTERN var_id =located(IDENTIFICATEUR) DPOINTS ts = located(typeScheme) {
  DeclareExtern(Position.map (fun v -> Id v) var_id,ts)
}
| vd = vdefinition {
  DefineValue vd
}

var:
|t = located(VARIABLE){
  Position.map (fun v -> TId v) t
}

tdefinition:
| BVERTICALE? constr_id = located(CONST_ID) {
  DefineSumType [(Position.map (fun v -> KId v) constr_id,[])]
}
| BVERTICALE? constr_id = located(CONST_ID) LPAR tl=separated_nonempty_list(COMMA,located(htype)) RPAR {
  DefineSumType [(Position.map (fun v -> KId v) constr_id,tl)]
}
| BVERTICALE? constr_id = located(CONST_ID) LPAR tl=separated_nonempty_list(COMMA,located(htype)) RPAR td = tdefinitioniter {
  DefineSumType ((Position.map (fun v -> KId v) constr_id,tl)::td)
}
| BVERTICALE? constr_id = located(CONST_ID) td = tdefinitioniter {
  DefineSumType ((Position.map (fun v -> KId v) constr_id,[])::td)
}
| LACC label_id_list=separated_nonempty_list(COMMA,label_id)  RACC{
  DefineRecordType label_id_list
}

label_id:
| id=located(IDENTIFICATEUR) DPOINTS t= located(htype) {
  (Position.map (fun v -> LId v) id,t)
}

(*J'ai fais ca car si la liste de tdef a un element le | est optionnel mais c'est pas le cas pour les tdef suivants*)
tdefinitioniter:
| BVERTICALE constr_id = located(CONST_ID) {
  [(Position.map (fun v -> KId v) constr_id,[])]
}
| BVERTICALE constr_id = located(CONST_ID) LPAR tl=separated_nonempty_list(COMMA,located(htype)) RPAR {
  [(Position.map (fun v -> KId v) constr_id,tl)]
}
| BVERTICALE constr_id = located(CONST_ID) LPAR tl=separated_nonempty_list(COMMA,located(htype)) RPAR td = tdefinitioniter {
  (Position.map (fun v -> KId v) constr_id,tl)::td
}
| BVERTICALE constr_id = located(CONST_ID) td = tdefinitioniter {
  (Position.map (fun v -> KId v) constr_id,[])::td
}

vdefinition:
| LET var_id=located(IDENTIFICATEUR) EQUAL exp=located(expr) {
  SimpleValue (Position.map (fun v -> Id v) var_id,None,exp)
} %prec VDEFPREC
| LET var_id=located(IDENTIFICATEUR) DPOINTS ts = located(typeScheme) EQUAL exp=located(expr){
  SimpleValue (Position.map (fun v -> Id v) var_id,Some ts,exp)
}  %prec VDEFPREC
| FUN fdl = separated_list(COMMA, fundef) {
  RecFunctions fdl
} 



fundef:
| var_id = located(IDENTIFICATEUR) p= located(pattern) EQUAL e=located(expr) {
  ((Position.map (fun v -> Id v ) var_id),None,FunctionDefinition (p,e))
}  %prec PVIRGULE
| DPOINTS ts = located(typeScheme) var_id = located(IDENTIFICATEUR) p= located(pattern) EQUAL e=located(expr) {
  ((Position.map (fun v -> Id v ) var_id),Some ts,FunctionDefinition (p,e))
}%prec PVIRGULE


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
|LPAR e=located(expr) COMMA el=separated_nonempty_list(COMMA,located(expr)) RPAR {
  Tuple(e::el)
}
| LPAR e1=located(expr) DPOINTS t=located(htype) RPAR {
  TypeAnnotation (e1,t)
}

|LPAR e=expr RPAR {
  e
}
|LPAR RPAR {
  Tuple([])
} 

|var_id=located(IDENTIFICATEUR) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON {
  Variable ((Position.map (fun v -> Id v) var_id),Some tl)
}
|var_id=located(IDENTIFICATEUR) {
  Variable ((Position.map (fun v -> Id v) var_id),None)
}

|LACC rl=separated_nonempty_list(COMMA,record) RACC LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON{
  Record(rl,Some tl)
}

|LACC rl=separated_nonempty_list(COMMA,record)  RACC {
  Record(rl,None)
}



app_chaine:
| e=located(expr_atomic) DOT label_id=located(IDENTIFICATEUR) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON {
  Field(e,Position.map (fun v -> LId v) label_id, Some tl)
}
| e=located(expr_atomic) DOT label_id=located(IDENTIFICATEUR) {
  Field(e,Position.map (fun v -> LId v) label_id, None)
}
| e = located(app_chaine) e1 =located(expr_atomic) {
  Apply(e,e1)
}
| e = expr_atomic {
  e
}
 
expr:

// |ea = expr_atomic { ea }
|ac = app_chaine { ac }

|constr_id=located(CONST_ID) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON LPAR el = separated_nonempty_list(COMMA,located(expr)) RPAR{
  Tagged ((Position.map (fun v -> KId v) constr_id),Some tl,el)
}

|constr_id=located(CONST_ID) LPAR el = separated_nonempty_list(COMMA,located(expr)) RPAR {
  Tagged ((Position.map (fun v -> KId v) constr_id),None,el)
}

|constr_id=located(CONST_ID) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON {
  Tagged ((Position.map (fun v -> KId v) constr_id),Some tl,[])
}
|constr_id=located(CONST_ID) {
  Tagged ((Position.map (fun v -> KId v) constr_id),None,[])
} 

| e = located(expr) b =located(binop) e1 = located(expr){
  let x = 
  Position.with_poss $startpos $endpos (Variable(b,None)) 
  in
    let y = Position.with_poss $startpos $endpos (Apply(x,e))
  in
  Apply(y,e1) 
}
//y'avait les record ici jlai tej en haut

| IF LPAR e1 = located(expr) RPAR THEN LACC e2 = located(expr) RACC ELSE LACC e3 = located(expr) RACC {
  IfThenElse(e1,e2,e3)
}

| IF LPAR e1 = located(expr) RPAR THEN LACC e2 = located(expr) RACC {
  IfThenElse(e1,e2,(Position.with_poss $startpos $endpos (Tuple([]))))
}

|WHILE LPAR e1=located(expr) RPAR LACC e2=located(expr) RACC {
  While (e1,e2)
}

| MATCH LPAR e=located(expr) RPAR LACC b= branchList RACC {
  Case(e,b)
}
|DO LACC e1=located(expr) RACC UNTIL LPAR e2=located(expr) RPAR {
  Sequence [e1; Position.with_poss $startpos $endpos (While(e2,e1))] (* Pas sur parce que techniquement until veux que While((!e2),e1) *)
}
| FOR var_id=located(IDENTIFICATEUR) FROM LPAR e1=located(expr) RPAR TO LPAR e2=located(expr) RPAR LACC e3=located(expr) RACC {
  For (Position.map (fun v -> Id v) var_id,e1,e2,e3)
}

| BACKSLASH p=located(pattern) RARROW e = located(expr) {
  Fun(FunctionDefinition(p,e))
}

| v = vdefinition PVIRGULE e=located(expr) 
// %prec VDEFPREC
{
  Define(v,e)
} 

| e=located(expr) PVIRGULE es=located(expr) { 
    Sequence([e; es])
} 

| e= located(expr) AFFECTATION e1 = located(expr) {
  Assign(e,e1)
}
| REF e= located(expr) %prec PREFIX {
  Ref(e)
}
| EXCLAMATION e=located(expr) %prec PREFIX {
  Read(e)
}



record:
| label_id=located(IDENTIFICATEUR) EQUAL e=located(expr){
  (Position.map (fun v -> LId v) label_id,e)
}


branchList :
| l = branches {
  l
}
| b = located(branche){
  [b]
}
| b = located(branche) l=branches {
  b::l
}

branches :
| BVERTICALE b = located(branche) l=branches {
  b::l
}
| BVERTICALE b = located(branche) {
  [b]
}

branche:
| p = located(pattern) RARROW e = located(expr) {
  Branch(p,e)
}

type_atomic :
|type_con=IDENTIFICATEUR LCHEVRON tl=separated_nonempty_list(COMMA,located(htype)) RCHEVRON {
  TyCon (TCon type_con,tl)
}
| tv = VARIABLE {
  TyVar (TId tv)
}
| LPAR t=htype RPAR { 
  t
}
|type_con=IDENTIFICATEUR {
  TyCon (TCon type_con,[])
}

uplet : 
| t=type_atomic {
  t
}
| t1=located(type_atomic) tu = typeUplet {
  TyTuple (t1::tu)
}

typeUplet : 
| STAR t=located(type_atomic) tu= typeUplet {
  (t):: tu
}
| STAR t=located(type_atomic) {
    [t]
}

htype:
| t1=located(uplet) RARROW t2=located(htype) {
  TyArrow (t1,t2)
}
| t1=uplet {
  t1
}%prec TEST



typeScheme :
| LCROCHET l=separated_nonempty_list(COMMA,var) RCROCHET t=located(htype) {
  ForallTy(l,t)
} 
| t=located(htype) {
  ForallTy([],t)
}

labelPattern:
| id= located(IDENTIFICATEUR) EQUAL p = located(pattern) {
  (Position.map (fun v -> LId v) id,p)
}

pattern_atomic :
| i = located(IDENTIFICATEUR) {
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

| LACC l = separated_nonempty_list(COMMA,labelPattern) RACC LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON {
  PRecord( l,Some tl)
}

| c = located(CONST_ID) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON LPAR l =separated_nonempty_list(COMMA,located(pattern)) RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, Some tl,l)
}

| c = located(CONST_ID) LCHEVRON tl=separated_list(COMMA,located(htype)) RCHEVRON  {
  PTaggedValue(Position.map (fun v -> KId v) c, Some tl,[])
}

| c = located(CONST_ID)  LPAR l = separated_nonempty_list(COMMA,located(pattern)) RPAR {
  PTaggedValue(Position.map (fun v -> KId v) c, None,l)
}

| c = located(CONST_ID) {
  PTaggedValue(Position.map (fun v -> KId v) c, None,[])
}

| LPAR p=pattern RPAR {
  p
}

| LPAR RPAR {
  PTuple([])
}
| LPAR pat= located(pattern) COMMA p=listpat RPAR {
  PTuple(pat::p)
}

| LACC l = separated_nonempty_list(COMMA,labelPattern) RACC {
  PRecord(l,None)
} 

listpat : 
| p= located(pattern) COMMA l=listpat{
  p::l
}
| p = located(pattern){
  [p]
}


pattern :
| p=pattern_atomic {
  p
}
| p=located(pattern) BVERTICALE pp=located(pattern) {
  POr(p ::[pp])
}

| p = located(pattern) DIS pp=located(pattern) {
  PAnd(p::[pp])
}

| p=located(pattern) DPOINTS t = located(htype){
  PTypeAnnotation(p,t)
}

%inline binop: 

| PLUS {
  Id("`+`")
}
| MOINS {
  Id("`-`") 
}
| STAR {
  Id("`*`")
}
| DIV {
   Id("`/`")
}
| OR {
 Id("`||`")
}
| AND {
  Id("`&&`")
}
| EQ {
  Id("`=?`")
}
| LTE {
  Id ("`<=?`")
}
| DRARROW {
   Id("`>=?`")
}
| LT {
  Id("`<?`")
}
| GT {
  Id("`>?`")
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
