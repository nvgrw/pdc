#include "print.h"

/**
 * The purpose of this object is to 'exercise' the stdlib so that clang does not
 * exclude the standard library functions. This is to convert a C signature info
 * an LLVM signature for interop.

#define NULL ((void*)0)

int main(int argc, char** argv) {
  print_newline();

  print_int(0);
  print_char(0);
  print_bool(0);
  print_float(0);

  print_array_int(NULL, 0, NULL);
  print_array_char(NULL, 0, NULL);
  print_array_bool(NULL, 0, NULL);
  print_array_float(NULL, 0, NULL);
  return 0;
}

