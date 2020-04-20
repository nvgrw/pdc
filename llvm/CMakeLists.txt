cmake_minimum_required (VERSION 2.6)
project (pdc_llvm_support)

find_package(LLVM REQUIRED CONFIG)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

add_subdirectory(A4RRandomProvider)
