%{ (* Emacs, open this with -*- tuareg -*- *)
open AST
%}

%token<int> INT
%token<string> ID
%token PLUS EOF FUN RIGHT_ARROW LP RP

%start<AST.expression> phrase

%nonassoc RIGHT_ARROW
%left PLUS

%nonassoc prec_application

%nonassoc LP INT ID FUN

%%

phrase:
  e=expression EOF { e }

arguments:
 | e=expression { [ e ] } %prec prec_application
 | hd=expression tl=arguments { hd::tl }

expression:
 | LP e=expression RP { e }
 | n=INT { Int n }
 | x=ID  { Var x }
 | e1=expression PLUS e2=expression { Add (e1, e2) }
 | FUN vars=nonempty_list(ID) RIGHT_ARROW e=expression
   { Fun { bound=vars; body=e }}
 | f=expression args=arguments
   { App (f, args) }
