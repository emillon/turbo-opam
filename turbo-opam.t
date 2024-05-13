  $ turbo-opam parse << EOF
  > depends: [
  >  ("a" {>= "3.0"} "b")
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > depends: [ "a" {build & <= "v"} ]
  > EOF

  $ turbo-opam parse << EOF
  > available: arch != "arm32" & arch != "x86_32"
  > EOF

  $ turbo-opam parse << EOF
  > depends: "p" { build }
  > EOF

  $ turbo-opam parse << EOF
  > depends: [ "a" | "b" ]
  > EOF

  $ turbo-opam parse --debug-ast << EOF
  > depends: "x" {>= "a" | >= "b"}
  > EOF
  [[depends]]
  V_filter (V_string "x", [V_op (Ge, V_or (V_string "a", V_op (Ge, V_string "b")))])
  

  $ turbo-opam parse << EOF
  > depends: ("a" "b")
  > EOF

  $ turbo-opam parse << EOF
  > available: !x
  > EOF

  $ turbo-opam parse << EOF
  > build: [
  >     "x" {with-test & ocaml:version < "5.2"}
  > ]
  > EOF
