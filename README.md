# _pdc_ (Probabilistic Dragon Compiler)

This README is a work in progress.

# Compilation

## One liner

1. `pdc -o /dev/stdout prog.pd | clang -x ir - -Lstdlib/build -lpdstd -o prog`

## Some separate files

1. `pdc -o /dev/stdout prog.pd | llc -filetype=obj -o prog.o`
2. `clang prog.o -Lstdlib/build -lpdstd -o prog`

## Separate files

1. `pdc -o prog.bc prog.pd`
2. `llc -filetype=obj prog.bc -o prog.o`
3. `clang prog.o -Lstdlib/build -lpdstd -o prog`

## _pdc_ only

_pdc_ could invoke the LLVM static compiler and clang itself. Currently it can't do this.