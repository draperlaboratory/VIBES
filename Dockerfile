FROM ocaml/opam:ubuntu-20.04-ocaml-4.14

ENV PATH=/home/opam/boolector/build/bin:/home/opam/MiniZincIDE-2.6.0-bundle-linux-x86_64/bin:/home/opam/.opam/4.14/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

ARG OPAM_JOBS=0

RUN if [ "${OPAM_JOBS}" -ne "0" ]; then \
      sed -i 's/jobs: [[:digit:]]\+/jobs: '${OPAM_JOBS}'/' /home/opam/.opam/config; \
    else \
       sed -i 's/jobs: [[:digit:]]\+/jobs: '$(nproc)'/' /home/opam/.opam/config; \
    fi

RUN opam repo add opam 'https://opam.ocaml.org/'

COPY --chown=opam:opam . vibes-tools

RUN opam config exec -- bash vibes-tools/bin/setup/ubuntu.bash

RUN opam config exec -- make -C vibes-tools/
