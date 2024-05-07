{
    type token =
        | Ident of string
        | Colon
        | String of string
        | Lbracket
        | Rbracket
        | Lbrace
        | Rbrace
        | Lt
        | Eq
        | Lparen
        | Rparen
        | Ge
        | Or
        | And

    let token_to_string = function
        | Ident s -> Printf.sprintf "Ident %S" s
        | Colon -> "Colon"
        | String s -> Printf.sprintf "String %S" s
        | Lbracket -> "Lbracket"
        | Rbracket -> "Rbracket"
        | Lbrace -> "Lbrace"
        | Rbrace -> "Rbrace"
        | Lt -> "Lt"
        | Eq -> "Eq"
        | Lparen -> "Lparen"
        | Rparen -> "Rparen"
        | Ge -> "Ge"
        | Or -> "Or"
        | And -> "And"
}

let ident = ['a'-'z''-']+

let space = [' ']+

rule token = parse
    | eof { None }
    | ident as s { Some (Ident s) }
    | ':' { Some Colon }
    | space { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | '"' { let s = string (Buffer.create 0) lexbuf in Some (String s) }
    | '[' { Some Lbracket }
    | ']' { Some Rbracket }
    | '{' { Some Lbrace }
    | '}' { Some Rbrace }
    | '<' { Some Lt }
    | '=' { Some Eq }
    | '#' { line_comment lexbuf }
    | '(' { Some Lparen }
    | ')' { Some Rparen }
    | ">=" { Some Ge }
    | '|' { Some Or }
    | '&' { Some And }

and string buf = parse
    | '"' { Buffer.contents buf }
    | _ as c { Buffer.add_char buf c; string buf lexbuf }

and line_comment = parse
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | _ { line_comment lexbuf }
