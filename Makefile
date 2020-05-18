PREFIX ?= /usr/local
PDC_FILES := $(shell find bin lib -name "*.ml" -or -name "*.mli")
STD_FILES := stdlib/test.c stdlib/llvm.c $(shell find stdlib/src stdlib/include -name "*.c" -or -name "*.h")

LLX_FILES := $(shell find llvm -name "*.cpp" -or -name "*.h")

.PHONY: all clean clean-all install

all: _build/install/default/bin/pdc

ifeq ($(DEBUG),1)
   CMAKE_OPTS = -DCMAKE_BUILD_TYPE=Debug
	 DUNE_OPTS = --profile=dev
 else
   CMAKE_OPTS = -DCMAKE_BUILD_TYPE=Release
	 DUNE_OPTS = --profile=release
endif

clean:
	dune clean
	rm -rf stdlib/build
	rm -rf llvm/build
	rm -f .deps-installed
	rm -f .prefix

clean-all: clean
	rm -f $(PREFIX)/bin/pdc
	rm -f $(PREFIX)/lib/libpdstd.a
	rm -f $(PREFIX)/lib/libA4RRP.so

install: _build/install/default/bin/pdc stdlib/build/libpdstd.a llvm/build/libA4RRP.so
	mkdir -p $(PREFIX)/bin
	cp _build/default/bin/pdc.exe $(PREFIX)/bin/pdc
	mkdir -p $(PREFIX)/lib
	cp stdlib/build/libpdstd.a $(PREFIX)/lib/libpdstd.a
	cp llvm/build/libA4RRP.so $(PREFIX)/lib/libA4RRP.so

.deps-installed:
	opam install . -y --deps-only && \
	touch .deps-installed

_build/install/default/bin/pdc: .deps-installed stdlib/build/llvm.bc stdlib/build/libpdstd.a llvm/build/libA4RRP.so $(PDC_FILES)
	rm -f .prefix && printf "$(PREFIX)" > .prefix
	dune build $(DUNE_OPTS)

stdlib/build/llvm.bc: stdlib/build/CMakeFiles/llvm.dir/llvm.c.o
	cp stdlib/build/CMakeFiles/llvm.dir/llvm.c.o stdlib/build/llvm.bc

stdlib/build/libpdstd.a stdlib/build/CMakeFiles/llvm.dir/llvm.c.o: stdlib/build/Makefile $(STD_FILES)
	cd stdlib/build && $(MAKE)

stdlib/build/Makefile: stdlib/CMakeLists.txt
	mkdir -p stdlib/build
	cd stdlib/build && cmake -DCMAKE_BUILD_TYPE=Release ..

llvm/build/libA4RRP.so: llvm/build/A4RRP/libA4RRP.so
	cp llvm/build/A4RRP/libA4RRP.so llvm/build/libA4RRP.so

llvm/build/A4RRP/libA4RRP.so: llvm/build/Makefile $(LLX_FILES)
	cd llvm/build && $(MAKE)

llvm/build/Makefile llvm/build/A4RRP/Makefile: llvm/CMakeLists.txt llvm/A4RRP/CMakeLists.txt
	mkdir -p llvm/build
	cd llvm/build && cmake $(CMAKE_OPTS) ..
