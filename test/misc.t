  $ turbo-opam parse << EOF
  > patches: ["a"]
  > EOF

  $ turbo-opam parse << EOF
  > patches: [ "a" { b = "c" } ]
  > EOF

  $ turbo-opam parse << EOF
  > patches: "a"
  > EOF

  $ turbo-opam parse << EOF
  > patches: "a" { b = "c" }
  > EOF
