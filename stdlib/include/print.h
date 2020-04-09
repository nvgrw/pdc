#include <stdbool.h>
#include <inttypes.h>

#include "common.h"

void STD(print_newline)(void);

void STD(print_int)(int64_t value);
void STD(print_char)(int8_t value);
void STD(print_bool)(bool value);
void STD(print_float)(double value);

void STD(print_array_int)(int64_t* array, int64_t n_dim, int64_t* dim);
void STD(print_array_char)(int8_t* array, int64_t n_dim, int64_t* dim);
void STD(print_array_bool)(bool* array, int64_t n_dim, int64_t* dim);
void STD(print_array_float)(double* array, int64_t n_dim, int64_t* dim);
