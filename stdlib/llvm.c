#include "print.h"
#include "random.h"

/**
 * The purpose of this object is to 'exercise' the stdlib so that clang does not
 * exclude the standard library functions. This is to convert a C signature info
 * an LLVM signature for interop. */

#define NULL ((void*)0)

void _exercise(void) {
  STD(print_newline)();

  STD(print_int)(0);
  STD(print_char)(0);
  STD(print_bool)(0);
  STD(print_float)(0);

  STD(print_array_int)(NULL, 0, NULL);
  STD(print_array_char)(NULL, 0, NULL);
  STD(print_array_bool)(NULL, 0, NULL);
  STD(print_array_float)(NULL, 0, NULL);

  STD(random)();
}

