
add_library(A4RRP MODULE Pass.cpp)

target_link_libraries(A4RRP ${LLVM_LIBS})

install(TARGETS A4RRP DESTINATION lib)
