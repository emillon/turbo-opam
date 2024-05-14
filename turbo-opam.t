  $ turbo-opam parse << EOF
  > depends: [
  >  ("a" {>= "3.0"} "b")
  > ]
  > EOF
  different result for string.0.opam: depends differs:
  a >= "3.0" & b
  (a >= "3.0" & b)
  [1]

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
  [[opam-version]]
  V_string "2.0"
  [[depends]]
  V_filter (V_string "x", [V_or (V_op (Ge, V_string "a"), V_op (Ge, V_string "b"))])
  

  $ turbo-opam parse << EOF
  > depends: ("a" "b")
  > EOF
  different result for string.0.opam: depends differs:
  a & b
  (a & b)
  [1]

  $ turbo-opam parse << EOF
  > available: !x
  > EOF

  $ turbo-opam parse << EOF
  > build: [
  >     "x" {with-test & ocaml:version < "5.2"}
  > ]
  > EOF

  $ turbo-opam parse --debug-ast << EOF
  > depends: x {a != "b" & c != "d" }
  > EOF
  [[opam-version]]
  V_string "2.0"
  [[depends]]
  V_filter (V_ident "x", [V_ident "a"; V_op (Neq, V_and (V_string "b", V_op2 (V_ident "c", Neq, V_string "d")))])
  
  compile error in string.0.opam: in filter: not a string
  [1]
  $ turbo-opam parse --debug-ast << EOF
  > depends: "a" {os != "macos" & os-family != "windows"}
  > EOF
  [[opam-version]]
  V_string "2.0"
  [[depends]]
  V_filter (V_string "a", [V_ident "os"; V_op (Neq, V_and (V_string "macos", V_op2 (V_ident "os-family", Neq, V_string "windows")))])
  
  different result for string.0.opam: depends differs:
  a {os & != ("macos" & os-family != "windows")}
  a {os != "macos" & os-family != "windows"}
  [1]

  $ turbo-opam parse << EOF
  > depends: [
  >   "ocaml" {>= "4.05.0"}
  >   "yojson" {< "2.0.0"}
  >   "xmlm"
  >   "ounit" {with-test}
  >   "lwt"
  >   "lwt_react"
  >   "obus" {os != "macos" & os-family != "windows"}
  >   "ocurl" {>= "0.7.9"}
  >   "sha" {>= "1.9"}
  >   "dune" {>= "1.11"}
  > ]
  > EOF
  different result for string.0.opam: depends differs:
  ocaml >= "4.05.0" & yojson < "2.0.0" & xmlm & ounit {with-test} & lwt & lwt_react & obus {os & != ("macos" & os-family != "windows")} & ocurl >= "0.7.9" & sha >= "1.9" & dune >= "1.11"
  ocaml >= "4.05.0" & yojson < "2.0.0" & xmlm & ounit {with-test} & lwt & lwt_react & obus {os != "macos" & os-family != "windows"} & ocurl >= "0.7.9" & sha >= "1.9" & dune >= "1.11"
  [1]
