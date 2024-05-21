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

  $ turbo-opam parse << EOF
  > extra-files: [
  >   ["a" "md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
  >   ["b" "md5=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > extra-files: ["a" "md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
  > EOF

  $ turbo-opam parse << EOF
  > substs: ["a"]
  > EOF

  $ turbo-opam parse << EOF
  > substs: "a"
  > EOF
