  $ turbo-opam parse << EOF
  > build: [
  >     "x" {with-test & ocaml:version < "5.2"}
  > ]
  > EOF
  different result for string.0.opam: build differs:
  [["x" { FAnd (FIdent ([], with-test, None), FOp (FIdent ([], ocaml:ocaml:version, None), <, FString _)) }]]
  [["x" { FAnd (FIdent ([], with-test, None), FOp (FIdent ([Some (ocaml)], version, None), <, FString _)) }]]
  [1]

  $ turbo-opam parse << EOF
  > build: [
  >   [make]
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > build: [
  >   [make "all"]
  >   [make "test"] {with-test}
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > build: [
  >   ["dune" "build" "-p" name "-j" jobs "@install" "@runtest" {with-test}]
  > ]
  > EOF

  $ turbo-opam parse << EOF
  > build: [ "a" ]
  > EOF

  $ turbo-opam parse << EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF
  different result for string.0.opam: build differs:
  [["dune"]; ["build"]; ["-p"]; [name]; ["-j"]; [jobs]]
  [["dune"; "build"; "-p"; name; "-j"; jobs]]
  [1]

  $ turbo-opam parse << EOF
  > build: "omake"
  > EOF

  $ turbo-opam parse << EOF
  > build: make
  > EOF

  $ turbo-opam parse << EOF
  > build: [
  >   [make] {ocaml:native}
  >   [make "all"] {!ocaml:native}
  > ]
  > EOF
  different result for string.0.opam: build differs:
  [[make] { FIdent ([], ocaml:ocaml:native, None) }; [make; "all"] { FNot _ }]
  [[make] { FIdent ([Some (ocaml)], native, None) }; [make; "all"] { FNot _ }]
  [1]

  $ turbo-opam parse << EOF
  > build: ["pkg-config" "libcurl"] {os != "macos"}
  > EOF
