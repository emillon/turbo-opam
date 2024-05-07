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
%token Eof

%left Or
%left And

%start<Ast.t> main;

%{ open Ast %}
%%

main: kvs Eof { $1 }

kvs:
| kv kvs { $1::$2}
| prekv Lbrace kvs Rbrace kvs { List.map (fun (k, v) -> $1::k, v) $3 @ $5  }
| { [] }

kv: Ident Colon value {[[$1]],$3}

prekv:
| Ident String { [$1; $2] }
| Ident { [$1] }

value: String { V_string $1 }
 | Lbracket values Rbracket { V_list $2 }
 | value Lbrace filter Rbrace { V_filter ($1, $3) }
 | filter { V_formula $1 }

values: value values { $1::$2 }
|  { [] }

filter:
| op value { F_op ($1, $2) }
| filter And filter {F_and ($1, $3)}
| filter Or filter {F_or ($1, $3)}
| Ident { F_ident $1 }
| value op2 value { F_op2 ($1, $2, $3) }
| Lparen filter Rparen { $2 }

op:
| Ge { Ge }
| Gt { Gt }
| Lt { Lt }
| Le { Le }
| Eq { Eq }

op2:
| Eq { Eq2 }
| Neq { Neq2 }
| Ge { Ge2 }
