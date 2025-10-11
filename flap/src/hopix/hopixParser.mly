%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}


%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA RARROW DIS
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

program: EOF
{  
  [] 

}


htype : 
|type_con=IDENTIFICATEUR t=htype {
  Tycon (TCon type_con,[located t])
}
|type_con=IDENTIFICATEUR LCHEVRON t=htype COMMA tl=typelist RCHEVRON {
  Tycon (TCon type_con,(located t)::tl)
}
| t1=htype RARROW t2=htype {
  TyArrow (located t1,located t2)
}
| LPAR t=htype RPAR {
  t
}
| t1=htype STAR tu = typeUplet{
  TyTuple(t1,tu)
}
| tv = VARIABLE {
  TyVar (TId tv)
}


typeUplet : 
| t=htype STAR tu= typeUplet {
  (located t):: located tu
}
| t=htype{
  [located t]
}

typelist : 
| t=htype COMMA tl=typelist {
  (located t)::tl
}
|t=htype{
    [located t]
}



typeScheme :
| LCROCHET l=typelist RCROCHET t=htype {
  ForallTy(l,located t)
} 
| t=htype {
  ForallTy(located t,[])
}



labelPatternList :
| id = IDENTIFICATEUR EQUAL p = pattern {
  [located (LId id), located p]
}

| id = IDENTIFICATEUR EQUAL p = pattern COMMA l=labelPatternList {
  (located (LId id) , located p) :: l
}


listPattern :
| p=pattern {
  [located p]
}
| p=pattern COMMA l = listPattern {
  (located p) :: (located l)
}

pattern :
| i = IDENTIFICATEUR {
  PVariable(located Id(i))
}
| b = BHORIZONTALE {
  PWildcard()
}
| i = ENTIER {
  PLiteral(located LInt(i))
  (* jsp si c located dedans ou dehors*)
}

| s = STRING {
  PLiteral(located LString(s))
}

| c = CHAR {
  PLiteral(located LChar(c))
}

| LPAR RPAR {PTuple([])}

| LPAR p=listPattern RPAR {
  PTuple(located p)
}

| p=pattern DPOINTS t = htype{
  PTypeAnnotation(located p,located t)
}

| p=pattern BVERTICALE pp=pattern {
  POr((located p) ::[located pp])
}
| p = pattern DIS pp=pattern {
  PAnd((located p)::[located pp])
}

| LPAR l = labelPatternList RPAR {
  PRecord(l,None)
}

| LPAR l = labelPatternList RPAR LCHEVRON t=typelist RCHEVRON {
  PRecord( l,Some t)
}


| c = CONST_ID LCHEVRON t=typelist RCHEVRON LPAR l = listPattern RPAR {
  PTaggedValue(located (KId c), Some t,l)
}

| c = CONST_ID {
  PTaggedValue(located (KId c),None,[])
}

| c = CONST_ID  LPAR l = listPattern RPAR {
  PTaggedValue(located (KId c), None,l)
}

| c = CONST_ID LCHEVRON t=typelist RCHEVRON  {
  PTaggedValue(located (KId c), Some t,[])
}

//TODO char
//RODOString


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
