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
