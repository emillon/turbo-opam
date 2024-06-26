let ident = ['A'-'Z''a'-'z''0'-'9''-''_']+

let space = [' ''\t']+

rule token = parse
    | eof { Parser.Eof }
    | ident as s { Parser.Ident s }
    | ident ':' ident { Parser.Ident (Lexing.lexeme lexbuf) }
    | ':' { Colon }
    | space { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "\"\"\"" { triple_string (Buffer.create 0) lexbuf }
    | '"' { string (Buffer.create 0) lexbuf }
    | '[' { Lbracket }
    | ']' { Rbracket }
    | '{' { Lbrace }
    | '}' { Rbrace }
    | '<' { Lt }
    | "<=" { Le }
    | '=' { Eq }
    | ">=" { Ge }
    | '#' { line_comment lexbuf; token lexbuf }
    | '(' { Lparen }
    | ')' { Rparen }
    | '|' { Or }
    | '&' { And }
    | "!=" { Neq }
    | '!' { Not }
    | '>' { Gt }
    | "(*" { ocaml_comment lexbuf; token lexbuf }
    | "+=" { PlusEq }

and string buf = parse
    | '"' { String (Buffer.contents buf) }
    | '\\' '"' { Buffer.add_char buf '\"'; string buf lexbuf }
    | '\\' 'n' { Buffer.add_char buf '\n'; string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; string buf lexbuf }
    | '\\' '\n' { Lexing.new_line lexbuf; string_spaces buf lexbuf }
    | '\n' { Lexing.new_line lexbuf; string buf lexbuf }
    | _ as c { Buffer.add_char buf c; string buf lexbuf }

and string_spaces buf = parse
    | space { string_spaces buf lexbuf }
    | _ as c { Buffer.add_char buf c; string buf lexbuf }

and triple_string buf = parse
    | "\"\"\"" { String (Buffer.contents buf) }
    | "\\\"" { Buffer.add_char buf '\"'; triple_string buf lexbuf }
    | '\n' { Buffer.add_char buf '\n';
             Lexing.new_line lexbuf;
             triple_string buf lexbuf
           }
    | _ as c { Buffer.add_char buf c; triple_string buf lexbuf }

and line_comment = parse
    | '\n' { Lexing.new_line lexbuf }
    | _ { line_comment lexbuf }

and ocaml_comment = parse
    | "*)" { () }
    | '\n' { Lexing.new_line lexbuf; ocaml_comment lexbuf }
    | _ { ocaml_comment lexbuf }
