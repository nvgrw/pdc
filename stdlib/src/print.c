#include <stdio.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdbool.h>

/*
| Array _ -> "%.0snot implemented\n"
| Int _ | Char _ | Bool _ -> "%d\n"
| Float _ -> "%f\n"
 */

void print_int(int64_t value) {
  printf("%lld", value);
}

void print_char(int8_t value) {
  printf("%d", value);
}

void print_bool(bool value) {
  if (value) {
    printf("true");
  } else {
    printf("false");
  }
}

void print_float(double value) {
  printf("%f", value);
}

static inline void _print_array(uint64_t* array, int n_dim, int* dim, int level) {
  if (n_dim == 1) {
    int array_size = *dim;
    printf("%*s[", level, "");
    for (int i = 0; i < array_size; i++) {
      printf("%lld", array[i]);
      if (i != array_size - 1) {
        printf(", ");
      }
    }
    printf("]");
    return;
  }

  int multiplier = 1;
  for (int i = 1; i < n_dim; i++) {
    multiplier *= dim[i];
  }

  int array_size = *dim;
  printf("%*s[\n", level, "");
  for (int i = 0; i < array_size; i++) {
    _print_array(&array[i * multiplier], n_dim - 1, dim + 1, level + 1);
    if (i != array_size - 1) {
      printf(",\n");
    } else {
      printf("\n");
    }
  }
  printf("%*s]", level, "");
}

void print_array(uint64_t* array, int n_dim, int* dim) {
  _print_array(array, n_dim, dim, 0);
}

