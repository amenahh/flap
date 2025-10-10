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
  
  let string_buffer =
    Buffer.create 13

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

let hexa = ['0'-'9' 'a'-'f''A'-'F']

let entier = '-'? digit+ | "0x" hexa+ |  "0b" ['0'-'1']+ | "0o" ['0'-'7']+ (*c ft *)

let atom = ['\000'-'\255'] | "Ox" hexa hexa | '\\' | '\'' | '\n' | '\t' | '\b' | '\r' 
let string = '"' (atom|"'"| '\"')* '"'

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (* Comment *)
  | openComment     { comment 1 lexbuf           }
  | lineComment     { token lexbuf               }
  (* String *)
  | '"'             { string lexbuf              }

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
  | "->" { RARROW }
  
  | "!"  { EXCLAMATION }
  | "&&" { AND }
  | "?"  { INTEROGATION }
  | "_"  { BHORIZONTALE }


  (* Litterals *)


  (*
  | '\'' ([^ '\\' '\''] as c) '\''{
  if (Char.code c < 32) then
    error lexbuf (
      Printf.sprintf
        "The ASCII character %d is not printable." (Char.code c)
    );
  ATOM c
  }*)
  | entier as e { ENTIER(e) }


  | identificateur as ident { IDENTIFICATEUR(ident) }
  | variable as v { VARIABLE(v) }
  | constr_id as c { CONST_ID(c) }


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

and string = parse 
  | atom as a { Buffer.add_string string_buffer a; string lexbuf}
  | '\''  { Buffer.add_char string_buffer '\''; string lexbuf}
  | "\""  { Buffer.add_char string_buffer '\"'; string lexbuf} 
  | '"'   { let s = Buffer.contents string_buffer in
            Buffer.clear string_buffer;
            STRING s}
  | eof   { error lexbuf "string is not closed."}