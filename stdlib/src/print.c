#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdbool.h>

#include "common.h"

void STD(print_newline)(void) {
  printf("\n");
}

void STD(print_int)(int64_t value) {
  printf("%" PRId64, value);
}

void STD(print_char)(int8_t value) {
  printf("%d", value);
}

void STD(print_bool)(bool value) {
  if (value) {
    printf("true");
  } else {
    printf("false");
  }
}

void STD(print_float)(double value) {
  printf("%f", value);
}

#define DEFINE_PRINT_ARRAY(NAME, TYPE)                                                \
static inline void _print_array_##NAME(TYPE* array, int64_t n_dim, int64_t* dim, int level) { \
  if (n_dim == 1) {                                                                   \
    int array_size = *dim;                                                            \
    printf("%*s[", level, "");                                                        \
    for (int i = 0; i < array_size; i++) {                                            \
      STD(print_##NAME)(array[i]);                                                    \
      if (i != array_size - 1) {                                                      \
        printf(", ");                                                                 \
      }                                                                               \
    }                                                                                 \
    printf("]");                                                                      \
    return;                                                                           \
  }                                                                                   \
                                                                                      \
  int multiplier = 1;                                                                 \
  for (int i = 1; i < n_dim; i++) {                                                   \
    multiplier *= dim[i];                                                             \
  }                                                                                   \
                                                                                      \
  int array_size = *dim;                                                              \
  printf("%*s[\n", level, "");                                                        \
  for (int i = 0; i < array_size; i++) {                                              \
    _print_array_##NAME(&array[i * multiplier], n_dim - 1, dim + 1, level + 1);       \
    if (i != array_size - 1) {                                                        \
      printf(", \n");                                                                 \
    } else {                                                                          \
      printf("\n");                                                                   \
    }                                                                                 \
  }                                                                                   \
  printf("%*s]", level, "");                                                          \
}                                                                                     \
                                                                                      \
void STD(print_array_##NAME)(TYPE* array, int64_t n_dim, int64_t* dim) {              \
  _print_array_##NAME(array, n_dim, dim, 0);                                          \
}

DEFINE_PRINT_ARRAY(int, int64_t)
DEFINE_PRINT_ARRAY(char, int8_t)
DEFINE_PRINT_ARRAY(bool, bool)
DEFINE_PRINT_ARRAY(float, double)
