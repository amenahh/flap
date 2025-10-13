{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser
  open Int64

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)
  
  let string_buffer =
    Buffer.create 10

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

let ascii = ['\000'-'\255']

let simpleatom = '\\' | '\'' | '\n' | '\t' | '\b' | '\r' 

let atom =  ascii | "Ox" hexa hexa | simpleatom

(*let string = '"' (atom|"'"| '\"')* '"' Obliger de faire une nouvelle regle pour ne pas capturer un '"' dans le string *)


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
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { LACC }
  | "}"  { RACC }
  | "["  { LCROCHET }
  |  "]" { RCROCHET }
  | ","  { COMMA }
  | "."  { DOT }
  | ";"  { PVIRGULE }
  
  (* binop *)
  | "\\" { BACKSLASH }
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
  | "&"  { DIS }
  | ":=" { AFFECTATION }
  
  | "!"  { EXCLAMATION }
  | "&&" { AND }
  | "?"  { INTEROGATION }
  | "_"  { BHORIZONTALE }


  (* Litterals *)
  | entier as e             { ENTIER(of_string e)  }
  | "'" (atom as a ) "'"    { let c = char_of_int(int_of_string a) in if (Char.code c) > 32 then CHAR(c)
                              else error lexbuf "Non printable char"
                            }


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
  | atom as a { 
    let c = char_of_int(int_of_string a) in 
    if (Char.code c) > 32 then (Buffer.add_char string_buffer (c); string lexbuf) (*Petite verif pour être sur que le char est affichable *)
    else error lexbuf "Non printable char"
    
    }
  | '\''  { Buffer.add_char string_buffer '\''; string lexbuf}
  | "\""  { Buffer.add_char string_buffer '\"'; string lexbuf} 
  | '"'   { let s = Buffer.contents string_buffer in
            Buffer.clear string_buffer;
            STRING s}
  | eof   { error lexbuf "string is not closed."}