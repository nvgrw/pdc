
add_library(SrandRP MODULE Pass.cpp)

target_link_libraries(SrandRP ${LLVM_LIBS})

install(TARGETS SrandRP DESTINATION lib)
