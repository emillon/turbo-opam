  $ turbo-opam parse << EOF
  > build: [
  >     "x" {with-test & ocaml:version < "5.2"}
  > ]
  > EOF

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

  $ turbo-opam parse << EOF
  > build: ["pkg-config" "libcurl"] {os != "macos"}
  > EOF

  $ turbo-opam parse << EOF
  > build: [make "all"]
  > EOF

  $ turbo-opam parse << EOF
  > build: []
  > EOF

  $ turbo-opam parse << 'EOF'
  > build: [
  >   [ "sh" "-ecx" "sed -e 's_@@ez-conf-lib:lib@@_%{_:lib}%_g' \
  >                      ez-conf-lib.config.in > ez-conf-lib.config" ]
  > ]
  > EOF

  $ turbo-opam parse << 'EOF'
  > build: [
  >   ["sh" "-c" """#!/bin/sh
  >     set -eufx
  >     if command -v brew; then
  >       eval $(brew shellenv)
  >       test -e Brewfile
  >     fi
  >    """]
  > ]
  > EOF
