# _pdc_ (Probabilistic Dragon Compiler)

This README is a work in progress.

# Installation

## Prerequisites

- GNU Make 3.81 (or newer)
- LLVM 9.0.0 with `clang` on path
- dune 1.11
- opam
- OCaml 4.07.1

## Building

The root of the repository contains a `Makefile` with the following non-file targets:

- **all**: compile pdc;
- **clean**: remove build-related files from repositor;
- **clean-all**: like clean but also removes installation;
- **install**: move pdc and libpdstd.a to /usr/local or `PREFIX` from environment.

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