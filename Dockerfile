FROM ubuntu:18.04

WORKDIR /root

RUN apt-get update
RUN apt-get install -y software-properties-common curl
RUN add-apt-repository ppa:avsm/ppa
RUN curl https://apt.kitware.com/keys/kitware-archive-latest.asc | gpg --dearmor - > /etc/apt/trusted.gpg.d/kitware.gpg
RUN apt-add-repository 'deb https://apt.kitware.com/ubuntu/ bionic main'

RUN apt-get update
RUN apt-get install -y opam m4 make clang cmake pkg-config

RUN opam init -a --disable-sandboxing
RUN opam switch create 4.07.1
RUN opam switch remove default
RUN opam install "dune>=2.0.0"

RUN git clone --depth=1 https://github.com/nvgrw/llvm-choose.git /tmp/llvm
RUN mkdir -p /tmp/llvm/llvm/build
WORKDIR /tmp/llvm/llvm/build
RUN cmake -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" -DLLVM_ENABLE_PROJECTS="clang" -DLLVM_ENABLE_IDE=OFF -DLLVM_TARGETS_TO_BUILD="X86" -DBUILD_SHARED_LIBS=ON -DLLVM_ENABLE_BINDINGS=OFF .. -Wno-dev
RUN make -j$((`nproc`+1))
RUN make install

ENV OPAM_SWITCH_PREFIX="/root/.opam/4.07.1"
ENV CAML_LD_LIBRARY_PATH="/root/.opam/4.07.1/lib/stublibs:Updated by package ocaml"
ENV OCAML_TOPLEVEL_PATH="/root/.opam/4.07.1/lib/toplevel"
ENV MANPATH=":/root/.opam/4.07.1/man"
ENV PATH="/root/.opam/4.07.1/bin:${PATH}"

WORKDIR /tmp/llvm/llvm/opam/llvm.10.0.0
RUN opam install -y .

WORKDIR /tmp/pdc
COPY . /tmp/pdc

RUN dune build @install || true
RUN opam install -y . --deps-only
RUN rm -rf /root/.opam/download-cache && rm -rf /root/.opam/default/.opam-switch/backup

RUN rm Makefile && mv Makefile.linux Makefile

ENV CC="clang"
ENV CXX="clang++"
RUN make && make install

# RUN rm -rf /var/lib/apt/lists/*
