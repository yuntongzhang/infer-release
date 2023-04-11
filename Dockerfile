# Infer built from source, from release version v1.1.0

FROM ubuntu:20.04

# dependecies from infer dockerfile
RUN apt-get update && \
    mkdir -p /usr/share/man/man1 && \
    apt-get install --yes --no-install-recommends \
      curl \
      libc6-dev \
      openjdk-11-jdk-headless \
      sqlite3 \
      xz-utils \
      zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# install opam
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends software-properties-common \
        build-essential \
        patch \
        git && \
    add-apt-repository ppa:avsm/ppa && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends opam

# install opam system-level dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends \
    autoconf \
    libgmp-dev \
    libsqlite3-dev \
    pkg-config \
    automake \
    cmake \
    clang \
    python \
    python3-distutils \
    libmpfr-dev

# get release version source code
WORKDIR /opt/
RUN git clone https://github.com/yuntongzhang/infer-release.git infer

# really building infer
# See https://bugs.llvm.org/show_bug.cgi?id=51359 on why clang is needed instead of gcc
WORKDIR /opt/infer/
ENV CC clang
ENV CXX clang++
RUN ./build-infer.sh -y clang
ENV PATH /opt/infer/infer/bin:${PATH}
