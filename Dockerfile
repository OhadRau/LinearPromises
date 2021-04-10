FROM ocaml/opam:alpine-ocaml-4.11
RUN opam install dune menhir

WORKDIR /usr/src/app
#COPY dune dune
#COPY dune-project dune-project
#COPY src/ src/
#COPY examples/ examples/
#COPY stackoverflow/ stackoverflow/
#COPY runtime/ runtime/
RUN git clone https://github.com/OhadRau/LinearPromises .
COPY ARTIFACT.md README.md

RUN eval $(opam config env) && dune build
RUN opam install ./linear-promises.opam

RUN sudo apk add openjdk11
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk
ENV PATH="$JAVA_HOME/bin:${PATH}"

RUN bash
