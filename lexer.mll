{
    type token = Parser.token

    let token_to_string =
        let open Parser in
        function
        | Ident s -> Printf.sprintf "Ident %S" s
        | Colon -> "Colon"
        | String s -> Printf.sprintf "String %S" s
        | Lbracket -> "Lbracket"
        | Rbracket -> "Rbracket"
        | Lbrace -> "Lbrace"
        | Rbrace -> "Rbrace"
        | Lt -> "Lt"
        | Le -> "Le"
        | Eq -> "Eq"
        | Lparen -> "Lparen"
        | Rparen -> "Rparen"
        | Ge -> "Ge"
        | Or -> "Or"
        | And -> "And"
        | Neq -> "Neq"
        | Not -> "Not"
        | Gt -> "Gt"
        | PlusEq -> "PlusEq"
}

let ident = ['A'-'Z''a'-'z''0'-'9''-''_']+

let space = [' ''\t']+

rule token = parse
    | eof { None }
    | ident as s { Some (Parser.Ident s) }
    | ':' { Some Colon }
    | space { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "\"\"\"" { triple_string (Buffer.create 0) lexbuf }
    | '"' { string (Buffer.create 0) lexbuf }
    | '[' { Some Lbracket }
    | ']' { Some Rbracket }
    | '{' { Some Lbrace }
    | '}' { Some Rbrace }
    | '<' { Some Lt }
    | "<=" { Some Le }
    | '=' { Some Eq }
    | '#' { line_comment lexbuf; token lexbuf }
    | '(' { Some Lparen }
    | ')' { Some Rparen }
    | ">=" { Some Ge }
    | '|' { Some Or }
    | '&' { Some And }
    | "!=" { Some Neq }
    | '!' { Some Not }
    | '>' { Some Gt }
    | "(*" { ocaml_comment lexbuf; token lexbuf }
    | "+=" { Some PlusEq }

and string buf = parse
    | '"' { Some (String (Buffer.contents buf)) }
    | "\\\"" { Buffer.add_char buf '\"'; string buf lexbuf }
    | "\\\\" { Buffer.add_char buf '\\'; string buf lexbuf }
    | '\n' { Lexing.new_line lexbuf; string buf lexbuf }
    | _ as c { Buffer.add_char buf c; string buf lexbuf }

and triple_string buf = parse
    | "\"\"\"" { Some (String (Buffer.contents buf)) }
    | "\\\"" { Buffer.add_char buf '\"'; triple_string buf lexbuf }
    | '\n' { Lexing.new_line lexbuf; triple_string buf lexbuf }
    | _ as c { Buffer.add_char buf c; triple_string buf lexbuf }

and line_comment = parse
    | '\n' { Lexing.new_line lexbuf }
    | _ { line_comment lexbuf }

and ocaml_comment = parse
    | "*)" { () }
    | '\n' { Lexing.new_line lexbuf; ocaml_comment lexbuf }
    | _ { ocaml_comment lexbuf }
