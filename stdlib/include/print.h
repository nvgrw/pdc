#include <stdbool.h>
#include <inttypes.h>

void print_newline(void);

void print_int(int64_t value);
void print_char(int8_t value);
void print_bool(bool value);
void print_float(double value);

void print_array_int(int64_t* array, int64_t n_dim, int64_t* dim);
void print_array_char(int8_t* array, int64_t n_dim, int64_t* dim);
void print_array_bool(bool* array, int64_t n_dim, int64_t* dim);
void print_array_float(double* array, int64_t n_dim, int64_t* dim);
