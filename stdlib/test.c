#include <inttypes.h>

#include "print.h"

int main(int argc, const char **argv) {
  int64_t ar[2][3][2] = {
    {{1, 9}, {2, 9}, {3, 9}},
    {{4, 9}, {5, 9}, {6, 9}}
  };
  int dimensions[3] = { 2, 3, 2 };
  print_array((uint64_t*) &ar, 3, dimensions);
  return 0;
}
