
add_library(Extra STATIC Extra.cpp)
find_package(OCaml REQUIRED)

target_include_directories(Extra PUBLIC ${OCAML_STDLIB_PATH})

target_link_libraries(Extra ${LLVM_LIBS})
