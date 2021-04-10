# Overview

This repository contains 3 relevant items:

1. The compiler & associated runtime
2. The example programs used for benchmarking the type checker
3. The translations of StackOverflow programs used in the case study

## Compiler/Runtime

The compiler exists inside of the src/ directory and the runtime exists
within the runtime/src/lang/promises/ directory. The compiler is already
installed as `linear-promises.compiler` if you're using the Docker image.
To build the compiler from scratch, you will need:

* A recent build of OCaml (I use 4.11.1, but >=4.08.0 should suffice)
* (Optional) The OPAM package manager
* The `dune` build system (easily installed through OPAM)
* The `menhir` library (easily installed through OPAM)

For the runtime, you will need a recent JDK (I use 11.0.5).

## Examples

The examples included in this directory were the programs used to benchmark
the typechecker. To simply compile the programs, you can run (from the root
project directory):

```
linear-promises.compiler -o <output file name> examples/<example name>.txt
```
This will compile the program with `<example name>` and output a Java program
at the specified output file path (you can choose not to specify the output
file, in which case it will default to `./<example name>.java`.

To instead perform the benchmarks, you can do either
```
# Typechecker benchmark
linear-promises.compiler --tybench examples/<example name>.txt
# Full pipeline benchmark
linear-promises.compiler --bench examples/<example name>.txt
```

Once your example program has been compiled, you can test it out by compiling
it to a Java program. The easiest way to do so is to simply copy the `.java`
file into the `runtime/src/` directory, then from that directory running
`javac <example name>.java`. To then run the program, `java <example name>`.

## Stackoverflow translations

These translations are all located in `stackoverflow/` and work similarly to
the example programs. In this directory, each program has the name
`q<stack overflow question ID>.txt`. These IDs match up with those in Table 2.
Compiling & running these programs is identical to the process for the example
programs.

## Example usage

To compile & run `examples/long.txt`:

```bash
$ linear-promises.compiler -o runtime/src/Long.java examples/long.txt
$ cd runtime/src
$ javac Long.java
$ java Long
```
