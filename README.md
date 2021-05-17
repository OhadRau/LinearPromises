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

Once your example program has been compiled, you can test it out by compiling
it to a Java program. The easiest way to do so is to simply copy the `.java`
file into the `runtime/src/` directory, then from that directory running
`javac <example name>.java`. To then run the program, `java <example name>`.

## Benchmarking programs

To instead perform the benchmarks, you can do either
```
# Typechecker benchmark
linear-promises.compiler --tybench examples/<example name>.txt
# Full pipeline benchmark
linear-promises.compiler --bench examples/<example name>.txt
```
Both options report the time in seconds per 1000 runs. For example, running a
typechecker benchmark will yield the amount of time (in seconds) required to
run the typechecker on the provided program 1000 times. Likewise, the full
benchmark will yield the amount of time (in seconds) required to run the full
compiler (minus file I/O) 1000 times.

The benchmarks in Table 1 of the paper are provided in microseconds per 1 run,
so to get comparable results you should divide by 1000 runs and convert seconds
to microseconds (multiply by 1000000).

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

## Language Syntax Guide

```
-- Types
Unit -- this is the nothing type, like void in some languages
Int
Bool
Promise(Type) -- read-handle for a promise of a value
Promise*(Type) -- write-handle for a promise of a value

-- Values
() -- Unit
1 -- Int
-5 -- Int
true -- Bool
false -- Bool
"hello" -- String

-- Functions
func <name>(<params>): <return> begin
  <code>
end

-- e.g.

func chooseFirstOne(a: Int, b: Int): Int begin
  a -- Functions automatically return the last value
end

-- Main function
func main(): Unit begin
  -- ...
end

-- Calling functions
<name>(<args>)

-- e.g.
f(1, true, p)

-- Asynchronous execution
async <expr>

-- e.g.
async f(1, true, p)

-- Let bindings
let <name>: <type> = <value> in
<code>

-- e.g.
let p: Bool = true in
p

-- Sequences
<code>; -- Sequence code with semi-colon
<code>  -- No semi-colon after last expression

-- Promises
-- Create a promise of an int (pRead is the read-handle, pWrite is the write-handle)
promise pRead, pWrite: Int in
-- pRead : Promise(Int) and pWrite : Promise*(Int)

-- Write value to promise (only works on write-handle)
pWrite <- 6;
-- After writing to pWrite, the variable no longer exists in scope
-- In fact, write-handles (and objects containing them) must be used _exactly_ once
-- We can still force a write using the unsafe write syntax (<~) on the read-handle
pRead <~ 7;

-- Await a value from promise's read-handle
?pRead

-- Conditionals
if <boolean expr> then
  <expr>
else
  <expr>
end

-- Loops
while <boolean expr> begin
  <expr>
end

for <name> = <initial value> to <final value> begin
  <expr>
end

-- User-defined types

-- Records
record <name> begin
  <field>: <type>;
  <field>: <type>...
end

-- e.g.
record Vector2d begin
  x: Int;
  y: Int
end

-- Construct a record
Vector2d { x=5, y=10 }

-- Access fields directly
func manhattanDistance(v: Vector2d): Int begin
  v.x + v.y
end

-- Or with pattern matching
func swap(v: Vector2d): Vector2d begin
  match v begin
    { x=a, y=b } -> Vector2d { x=b, y=a }
  end
end

-- Unions
union <name> begin
  <case>[<param types>];
  <case>[<param types>]...
end

-- e.g.
union List begin
  Nil[];
  Cons[Int, List]
end

-- Construct a union
Cons[1, Cons[2, Cons[3, Nil[]]]]

-- Deconstruct using pattern matching
func length(list: List): Int begin
  match list begin
    Nil[] -> 0
    Cons[x, rest] -> 1 + length(rest)
  end
end
```
