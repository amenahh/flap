%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}


%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA RARROW
%token EQUAL LCHEVRON RCHEVRON DPOINTS BVERTICALE MOINS EXCLAMATION PLUS REF 
%token INTEROGATION BHORIZONTALE DIV STAR LT GT DRARROW EQ OR LTE
%token <string> VARIABLE
%token <string> CONST_ID
%token <string> IDENTIFICATEUR
%token <string> ENTIER
%token <char> ATOM 

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


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
