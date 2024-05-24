# `turbo-opam`

## What is this?

This is an experiment to see how fast we can make opam file parsing by making a custom parser instead of the stock one based on `OpamPp`.

The vanilla pipeline is:

- `OpamLexer` (from `opam-file-format`)
- `OpamBaseParser` (from `opam-file-format`)
- `OpamParserTypes` AST
- `OpamFile` (from `opam-format`), using `OpamPp`, producing
  `OpamFile.OPAM.t`

This contains an alternate pipeline:

- `Lexer` (custom)
- `Parser` (custom)
- `Ast` (custom)
- `Compile`, producing `OpamFile.OPAM.t`

## Running

This is a nix flake :snowflake: that defines a program that has two main
modes:
- `turbo-opam repo /path/to/opam-repository` will parse a whole repository with both pipelines and report issues.
- `turbo-opam bench /path/to/opam-repository --parser <control|experiment>` will parse a whole repository with a single parser. Use this to run benchmarks, e.g:

```
hyperfine
  './_build/install/default/bin/turbo-opam bench ~/src/opam-repository --parser control'
  './_build/install/default/bin/turbo-opam bench ~/src/opam-repository --parser experiment'
```

(this is packaged as `nix build .#bench`)

## Results

The experimental pipeline is 1.8x faster than the vanilla one.

```
Summary
  ./_build/install/default/bin/turbo-opam bench ~/src/opam-repository --parser experiment ran
    1.80 ± 0.09 times faster than ./_build/install/default/bin/turbo-opam bench ~/src/opam-repository --parser control
```

## Future work

- compare with just replacing the `Compile` step. The goal is to minimize what we need to replace.
- optimize the alternative pipeline (for example, use exceptions instead of `Result` monad). The goal is to be fast, after all.
