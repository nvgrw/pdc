PREFIX ?= /usr/local
PDC_FILES := $(shell find bin lib -name "*.ml" -or -name "*.mli")
STD_FILES := stdlib/test.c stdlib/llvm.c $(shell find stdlib/src stdlib/include -name "*.c" -or -name "*.h")

.PHONY: all clean clean-all install

all: _build/install/default/bin/pdc

clean:
	dune clean
	rm -rf stdlib/build
	rm -f .deps-installed
	rm -f .prefix

clean-all: clean
	rm -f $(PREFIX)/bin/pdc
	rm -f $(PREFIX)/lib/libpdstd.a

install: _build/install/default/bin/pdc stdlib/build/libpdstd.a
	mkdir -p $(PREFIX)/bin
	cp _build/install/default/bin/pdc $(PREFIX)/bin/pdc
	mkdir -p $(PREFIX)/lib
	cp stdlib/build/libpdstd.a $(PREFIX)/lib/libpdstd.a

.deps-installed:
	opam install . --deps-only && \
	touch .deps-installed

_build/install/default/bin/pdc: .deps-installed stdlib/build/llvm.bc stdlib/build/libpdstd.a $(PDC_FILES)
	rm -f .prefix && printf "$(PREFIX)" > .prefix
	dune build

stdlib/build/llvm.bc: stdlib/build/CMakeFiles/llvm.dir/llvm.c.o
	cp stdlib/build/CMakeFiles/llvm.dir/llvm.c.o stdlib/build/llvm.bc

stdlib/build/libpdstd.a stdlib/build/CMakeFiles/llvm.dir/llvm.c.o: stdlib/build/Makefile $(STD_FILES)
	cd stdlib/build && $(MAKE)

stdlib/build/Makefile: stdlib/CMakeLists.txt
	mkdir -p stdlib/build
	cd stdlib/build && cmake ..
