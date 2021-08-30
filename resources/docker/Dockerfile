FROM binaryanalysisplatform/bap:latest

ENV DEBIAN_FRONTEND=noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN=true
ENV LD_LIBRARY_PATH=/home/bap/.opam/4.09/lib/z3
ENV OPAM_JOBS=3

RUN sed -i 's/jobs: [[:digit:]]\+/jobs: '${OPAM_JOBS}'/' /home/opam/.opam/config

COPY --chown=opam:opam . /home/opam/VIBES/
RUN sudo apt install -y qemu binutils-arm-linux-gnueabi gcc-arm-linux-gnueabi cmake

WORKDIR /home/opam/VIBES
RUN opam exec -- bash bin/setup/install-dependencies.bash

ENV PATH=/home/opam/boolector/build/bin:/home/opam/MiniZincIDE-2.5.3-bundle-linux-x86_64/bin:/home/opam/.opam/4.09/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN opam exec -- make
