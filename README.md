# _pdc_ (Probabilistic Dragon Compiler)

# Installation

## Prerequisites

- GNU Make 3.81 (or newer)
- LLVM 10.0.0 with `clang` on path
- dune 2
- opam
- OCaml 4.07.1

## Building

The root of the repository contains a `Makefile` with the following non-file
targets:

- **all**: compile pdc;
- **clean**: remove build-related files from repositor;
- **clean-all**: like clean but also removes installation;
- **install**: move pdc and libpdstd.a to /usr/local or `PREFIX` from environment.

Only tested on macOS.

Linux is currently unsupported, but I added some special steps to make it work
with Docker. If you wish to use Docker, run `docker build . -t pdc:latest` in
this directory, followed by an appropriate `docker run` invocation, like `docker
run --rm -it pdc:latest /bin/bash` for an interactive shell. You will be dropped
into `/root` with the `pdc` executable on the path. The build directory is
`/tmp/pdc`.

# Usage

## Compile .pd to executable

```
pdc program.pd -o program
```

## Compile .pd to LLVM bitcode

```
pdc program.pd -o program.bc
```

## Compile .pd to target object

```
pdc -c program.pd -o program
```
