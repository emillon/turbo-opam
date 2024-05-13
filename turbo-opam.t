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

  $ turbo-opam parse << EOF
  > available: arch != "arm32" & arch != "x86_32"
  > EOF

  $ turbo-opam parse << EOF
  > depends: "p" { build }
  > EOF
