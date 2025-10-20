{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser
  open Mint

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

let suite = (majuscule | minuscule | digit | '_')* 

let identificateur = minuscule suite

let variable = '`' identificateur 

let constr_id = majuscule suite

let hexa = ['0'-'9' 'a'-'f' 'A'-'F']

let entier = '-'? digit+ | "0x" hexa+ |  "0b" ['0'-'1']+ | "0o" ['0'-'7']+ 

(* let ascii = ['\000'-'\255'] *)

(* let simpleatom = '\\' | '\'' | '\n' | '\t' | '\b' | '\r' 

let atom =  ascii | "0x" hexa hexa | simpleatom

let string = '"' (atom|"'"| '\"')* '"'  *)
(* Obliger de faire une nouvelle regle pour ne pas capturer un '"' dans le string *)


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

  | entier as e { 
    let x =
    try int_of_string e with 
    | Failure _ -> error lexbuf "Integer literal too large."
    in 
    ENTIER( Mint.of_int x )
  }  
   | identificateur as ident { IDENTIFICATEUR(ident) }
  | variable as v { VARIABLE(v) }
  | constr_id as c { CONST_ID(c) }
  
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
  | "_"  { BHORIZONTALE }


  (* Litterals *)
  | "'\\n'"                 { CHAR '\n'                          }
  | "'\\t'"                 { CHAR '\t'                          }
  | "'\\''"                 { CHAR '\''                          }
  | "'\\\\'"                { CHAR '\\'                          }
  | "'\\r'"                 { CHAR '\r'                          }
  | "'\\b'"                 { CHAR '\b'                          }
  | "'\\0x" (hexa hexa as n ) "'" {
  let c = int_of_string ("0x" ^ n) in
  if  c >= 0 && c <= 255 then CHAR (char_of_int c)
  else error lexbuf "Bad hexa char"
  }
  | "'\\" (digit digit digit as i) "'" {
  let c = int_of_string i in
  if c >= 0 && c <= 255 then CHAR (char_of_int c)
  else error lexbuf "Bad octal char"
  }
  | "'" ([^'\'''\\'] as c) "'"  
  { 
    if (Char.code c) > 32 then CHAR(c)
    else error lexbuf "Non printable char"
  }




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
  | "\\n"                                 { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\t"                                 { Buffer.add_char string_buffer '\t'; string lexbuf }
  | "\\'"                                 { Buffer.add_char string_buffer '\''; string lexbuf }
  | "\\\""                                { Buffer.add_char string_buffer '"';  string lexbuf  }
  | "\\r"                                 { Buffer.add_char string_buffer '\r'; string lexbuf }
  | "\\b"                                 { Buffer.add_char string_buffer '\b'; string lexbuf }
  | "\\\\"                                { Buffer.add_char string_buffer '\\'; string lexbuf }
  | "\\0x" (hexa hexa as n )  {
  let c = int_of_string ("0x" ^ n) in
  if c >= 0 && c <= 255 then (Buffer.add_char string_buffer (char_of_int c) ; string lexbuf)
  else error lexbuf "Bad hexa in string"
  }
  | "\\" (digit digit digit as i) {
  let c = int_of_string i in
  if c >= 0 && c <= 255 then (Buffer.add_char string_buffer (char_of_int c) ; string lexbuf)
  else error lexbuf "Bad octal in string"
  }
  | '"'   { let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      STRING s
  }
  | _ as c { 
      Buffer.add_char string_buffer c; string lexbuf 
  }
  | eof   { error lexbuf "Unterminated string."}