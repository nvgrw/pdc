cmake_minimum_required (VERSION 3.4.3)
project (pdc_llvm_support)
set (CMAKE_CXX_STANDARD 14)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_CXX_EXTENSIONS OFF)
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules")

find_package (LLVM REQUIRED CONFIG)

message (STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message (STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions (${LLVM_DEFINITIONS})
include_directories (${LLVM_INCLUDE_DIRS})

llvm_map_components_to_libnames (LLVM_LIBS support core ipo scalarOpts)

if (NOT LLVM_ENABLE_RTTI)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

if (NOT LLVM_ENABLE_EH)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
endif()

add_subdirectory (A4RRP)
add_subdirectory (Extra)
