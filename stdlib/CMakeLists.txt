cmake_minimum_required (VERSION 3.4.3)
project (pdc_standard_library)

file (GLOB STD_SOURCES "src/*.c" "include/*.h")

add_library (pdstd STATIC "${STD_SOURCES}")
include_directories(include)

add_executable (test "test.c")
target_link_libraries (test pdstd)

target_compile_options(pdstd PUBLIC ${CMAKE_C_FLAGS} -Wall -Werror -pedantic)
target_compile_options(test PUBLIC ${CMAKE_C_FLAGS} -Wall -Werror -pedantic)

add_library (llvm OBJECT "llvm.c")
target_compile_options(llvm PUBLIC ${CMAKE_C_FLAGS} -c -flto)
