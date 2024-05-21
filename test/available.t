  $ turbo-opam parse << EOF
  > available: arch != "arm32" & arch != "x86_32"
  > EOF

  $ turbo-opam parse << EOF
  > available: !x
  > EOF

  $ turbo-opam parse << EOF
  > available: false
  > EOF

  $ turbo-opam parse << EOF
  > available: true
  > EOF

  $ turbo-opam parse << EOF
  > available: a >= 0
  > EOF
