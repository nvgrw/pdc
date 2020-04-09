#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>

#include "common.h"

double STD(random)(void) {
  const uint64_t rnd = (uint64_t) arc4random() + (((uint64_t) arc4random()) << 32);
  return (double) rnd / (double) UINT64_MAX;
}
