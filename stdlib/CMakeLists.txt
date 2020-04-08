cmake_minimum_required (VERSION 2.6)
project (pdc_standard_library)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Werror -pedantic")

file (GLOB STD_SOURCES "src/*.c" "include/*.h")

add_library (pdstd STATIC "${STD_SOURCES}")
include_directories(include)

add_executable (test "test.c")
target_link_libraries (test pdstd)
