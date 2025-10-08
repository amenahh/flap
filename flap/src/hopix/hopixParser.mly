%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}


%token EOF IF WHILE LET FUN TYPE EXTERN AND MATCH THEN ELSE DO UNTIL FOR FROM TO
%token LPAR RPAR LCROCHET RCROCHET COMMA 
%token EQUAL LCHEVRON RCHEVRON DPOINTS BVERTICALE MOINS EXCLAMATION PLUS REF
%token INTEROGATION BHORIZONTALE DIV STAR PLUS LT GT DRARROW EQ 
%token <string> VARIABLE
%token <string> CONST_ID
%token <string> IDENTIFICATEUR
%token <string> ENTIER
%token <string> ATOM 

%start<HopixAST.t> program

%%

program: EOF
{
   []
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
