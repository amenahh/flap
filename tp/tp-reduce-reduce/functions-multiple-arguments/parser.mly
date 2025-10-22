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

(* This grammar has reduce/reduce conflicts: resolve them.

Note that:
 - "f a1 a2 a3 a4" should give: App(Var "f", [Var "a1"; Var "a2"; Var "a3"; Var "a4"])
 - but "(f a1 a2) a3 a4" should give: App(App(Var "f", [Var "a1"; Var "a2"]), [Var "a3"; Var "a4"])

*)

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
