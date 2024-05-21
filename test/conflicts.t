  $ turbo-opam parse << EOF
  > conflicts: [
  >   "a" { ! (>= "b" & < "c") }
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > conflict-class: "ocaml-core-compiler"
  > EOF
