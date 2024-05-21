  $ turbo-opam parse << EOF
  > available: arch != "arm32" & arch != "x86_32"
  > EOF

  $ turbo-opam parse << EOF
  > available: !x
  > EOF
