  $ turbo-opam parse << EOF
  > conflicts: [
  >   "a" { ! (>= "b" & < "c") }
  > ]
  > EOF
