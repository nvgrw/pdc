#include <inttypes.h>

#include "print.h"

int main(int argc, const char **argv) {
  {
    int64_t a[2][3][2] = {
      {{1, 9}, {2, 9}, {3, 9}},
      {{4, 9}, {5, 9}, {6, 9}}
    };
    int64_t dim[] = { 2, 3, 2 };
    print_array_int((int64_t*) &a, 3, dim);
  }

  {
    bool a[1][4][4] = { {
      {false, false, false, true},
      {false, false, true, false},
      {false, true, false, false},
      {true, false, false, false}
    } };
    int64_t dim[] = { 1, 4, 4 };
    print_array_bool((bool*) &a, 3, dim);
  }
  return 0;
}
