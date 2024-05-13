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

main:
| kvs Eof
  { let sections=$1 in
    let filename = $startpos.Lexing.pos_fname in
    {sections; filename}
  }

kvs:
| kv kvs { $1::$2}
| prekv Lbrace kvs Rbrace kvs { List.map (fun (k, v) -> $1::k, v) $3 @ $5  }
| { [] }

kv:
| Ident Colon value {[[$1]],$3}

prekv:
| Ident String { [$1; $2] }
| Ident { [$1] }

value:
| String { V_string $1 }
| Lbracket values Rbracket { V_list $2 }
| value Lbrace value Rbrace { V_filter ($1, $3) }
| op value { V_op ($1, $2) }
| value And value {V_and ($1, $3)}
| value Or value {V_or ($1, $3)}
| Ident { V_ident $1 }
| value op2 value { V_op2 ($1, $2, $3) }
| Lparen value Rparen { $2 }

values:
| value values { $1::$2 }
| { [] }

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
