
add_library(Extra SHARED Extra.cpp)
add_library(ExtraStatic STATIC Extra.cpp)
find_package(OCaml REQUIRED)

set_target_properties(ExtraStatic PROPERTIES OUTPUT_NAME "Extra")
set_target_properties(Extra PROPERTIES PREFIX "dll")
set_target_properties(Extra PROPERTIES SUFFIX ".so")
set_target_properties(Extra PROPERTIES INSTALL_RPATH "${LLVM_LIBRARY_DIRS};${CMAKE_CURRENT_BINARY_DIR}")
target_link_options(Extra PUBLIC -Wl,-undefined,dynamic_lookup)
target_compile_options(Extra PUBLIC -fPIC)
target_compile_options(ExtraStatic PUBLIC -fPIC)

target_include_directories(Extra PUBLIC ${OCAML_STDLIB_PATH})
target_include_directories(ExtraStatic PUBLIC ${OCAML_STDLIB_PATH})

target_link_libraries(Extra PUBLIC ${LLVM_LIBS})
target_link_libraries(ExtraStatic PUBLIC ${LLVM_LIBS})
