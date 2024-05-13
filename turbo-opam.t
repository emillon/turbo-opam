  $ turbo-opam parse << EOF
  > depends: [
  >  ("a" {>= "3.0"} "b")
  > ]
  > EOF
  parse error in  near:
  2 |  ("a" {>= "3.0"} "b")
                          ^
  [1]

  $ turbo-opam parse << EOF
  > depends: [ "a" {build & <= "v"} ]
  > EOF

  $ turbo-opam parse --debug-token << EOF
  > available: arch != "arm32" & arch != "x86_32"
  > EOF
  Ident
  Colon
  Ident
  Neq
  parse error in  near:
  1 | available: arch != "arm32" & arch != "x86_32"
                        ^
  [1]
