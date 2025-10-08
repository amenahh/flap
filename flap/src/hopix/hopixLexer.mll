{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

}

let lineComment = "##" [^'\n']*
let openComment = "{*"
let closeComment = "*}"

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let minuscule = ['a'-'z']

let majuscule = ['A'-'Z']

let identificateur = minuscule (majuscule | minuscule | digit | '_')*  (* c ft *)

let variable = '`' identificateur (* c ft *)

let constr_id = majuscule identificateur (*c ft*)

let entier = '-'? digit+ | "0x" ['0'-'9' |'a'-'f''A'-'F']+ |  "0b" ['0'-'1']+ | "0o" ['0'-'7']+ (*c ft *)



rule token = parse
  (** Layout *)
  (* mot clés *)
  | "if" { IF }
  | "while" { WHILE }
  | "let" { LET }
  | "fun" { FUN }
  | "type" { TYPE }
  | "extern" { EXTERN }
  | "and" { AND }
  | "match" { MATCH }
  | "then" { THEN }
  | "else" { ELSE }
  | "do" { DO }
  | "until" { UNTIL }
  | "for" { FOR }
  | "from" { FROM }
  | "to" { TO }
  | "ref" { REF }
  
  (* ponctuation *)
  | "{"  { LPAR }
  | "}"  { RPAR }
  | "["  { LCROCHET }
  |  "]" { RCROCHET }
  | ","  { COMMA }
  
  (* binop *)
  | "+"  { PLUS }
  | "-"  { MOINS }
  | "*" { STAR }
  | "/" { DIV }
  | "||" { OR }
  | "=?" { EQ }
  | "<=?" { LTE }
  | ">=?" { DRARROW }
  | "<?" { LT }
  | ">?" { GT }
  
  | "="  { EQUAL }
  | "<"  { LCHEVRON }
  | ">"  { RCHEVRON }
  | ":"  { DPOINTS }
  | "|"  { BVERTICALE }
  
  | "!"  { EXCLAMATION }
  | "&&" { AND }
  | "?"  { INTEROGATION }
  | "_"  { BHORIZONTALE }
  | identificateur ad ident { IDENTIFICATEUR(ident) }
  | variable as v { VARIABLE(v) }
  | constr_id as c { CONST_ID(c) }
  | entier as e { ENTIER(e) }
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | openComment     { comment 1 lexbuf           }
  | lineComment     { token lexbuf               }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and comment cpt  = parse 
  | closeComment {
    if cpt-1 = 0 then token lexbuf
    else (comment (cpt-1) lexbuf)
    }
  | openComment  { comment (cpt+1) lexbuf }
  | eof          { error lexbuf "comment not closed."}
  | _            { comment cpt lexbuf     }