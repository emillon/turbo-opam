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

  $ turbo-opam parse << EOF
  > depends: "x" {>= "a" | >= "b"}
  > EOF

  $ turbo-opam parse << EOF
  > depends: ("a" "b")
  > EOF
