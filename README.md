# _pdc_ (Probabilistic Dragon Compiler)

This README is a work in progress.

# Compilation

## More pipes
1. `pdc -o /dev/stdout prog.pd | llc -filetype=obj -o prog.o`
2. `clang prog.o -o prog`

## No intermediate files
1. `pdc -o /dev/stdout prog.pd | clang -x ir - -o prog`

Appears to emit a warning:

```
warning: overriding the module target triple with x86_64-apple-macosx10.15.0 [-Woverride-module]
1 warning generated.
```

## Separate files

1. `pdc -o prog.bc prog.pd`
2. `llc -filetype=obj prog.bc -o prog.o`
3. `clang prog.o -o prog`

## _pdc_ only

_pdc_ could invoke the LLVM static compiler and clang itself. Currently it can't do this.