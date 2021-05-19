FROM ocaml/opam:alpine-ocaml-4.12
WORKDIR /usr/src/app
#COPY dune dune
#COPY dune-project dune-project
#COPY src/ src/
#COPY examples/ examples/
#COPY stackoverflow/ stackoverflow/
#COPY runtime/ runtime/
RUN git clone https://github.com/OhadRau/LinearPromises .

RUN rm -rf ~/.opam/
RUN opam init
RUN opam install dune menhir
RUN eval $(opam config env) && dune build
RUN opam install ./linear-promises.opam

RUN sudo apk add openjdk11
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk
ENV PATH="$JAVA_HOME/bin:${PATH}"

RUN bash
