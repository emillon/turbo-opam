%token<string> Ident
%token Colon
%token<string> String
%token Lbracket
%token Rbracket
%token Lbrace
%token Rbrace
%token Lt
%token Le
%token Eq
%token Lparen
%token Rparen
%token Ge
%token Or
%token And
%token Neq
%token Not
%token Gt
%token PlusEq
%start<Ast.t> main;
%%

main: kvs { $1 }

kvs: kv kvs { $1::$2}
| Ident Lbrace kvs Rbrace { List.map (fun (k, v) -> $1::k, v) $3  }
|  { [] }

kv: Ident Colon value {[$1],$3}

value: String { Ast.V_string $1 }
 | Lbracket values Rbracket { Ast.V_list $2 }
 | Ident { Ast.V_ident $1 }
 | Ident Colon Ident { Ast.V_var ($1, $3) }
 | value Lbrace filter Rbrace { Ast.V_filter ($1, $3) }

values: value values { $1::$2 }
|  { [] }

filter:
| op value { Ast.F_op ($1, $2) }
| filter And filter {Ast.F_and ($1, $3)}
| filter Or filter {Ast.F_or ($1, $3)}
| Ident { Ast.F_ident $1 }
| value op2 value { Ast.F_op2 ($1, $2, $3) }

op:
| Ge { Ast.Ge }
| Lt { Ast.Lt }
| Le { Ast.Le }
| Eq { Ast.Eq }

op2:
| Eq { Ast.Eq2 }
| Neq { Ast.Neq2 }
| Ge { Ast.Ge2 }
