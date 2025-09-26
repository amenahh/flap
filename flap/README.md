# The Flap Compiler

## Prerequisites

Flap requires **OCaml 5.1+**, as well as the tools and libraries listed
below. They should be installed prior to attempting to build Flap.

- The **dune** build system.
- The **utop** enhanced interactive read-eval-print-loop.
- Various libraries and tools for parsing (**menhir**), pretty-printing
  (**pprint**), serialization, and testing.

The easiest way to install all of them is via OPAM.

```shell
$ opam install dune utop
$ dune build flap.opam
$ opam install --deps-only --with-test -y .
```

## Build instructions

To compile the compiler, run `dune build` from this directory. This will compile
both the compiler itself (`flap`) and its test runner (`flaptest`).

To run the compiler, run `dune exec ./src/flap.exe -- [OPTIONS] file` from this
directory. This will recompile `flap` if needed.

Alternatively, the `flap.exe` binary can be found in `src/`.

## Testing the compiler

The test suite can be found in the `tests` directory.

To run the tests, use the `flaptest.exe` parsing.

```shell
$ dune exec ./testing/flaptest.exe -- tests                 # Run all tests
$ dune exec ./testing/flaptest.exe -- tests --only ko       # Only show failures
$ dune exec ./testing/flaptest.exe -- tests/01-Parsing      # Only test parsing
```
