# {Unnamed Language}

This repo is a compiler for {Unnamed Language} that utilizes substructural typing to track promise fulfillment.
In the future, this will evolve to track promise ownership so that we can perform deadlock detection.

[Overview of the type system](https://www.overleaf.com/read/pmqpspcscdpq) -- lots of churn incoming so this may
not reflect the current state of the type system until things are a little more finalized.

## How to use

Build with `dune build src/relprom.exe` in the root directory. You can compile programs with
`dune exec src/relprom.exe <filename>`, which will output the generated code on the command line. If you want to
use that code, redirect stdout to a file and remove the first few lines to get a valid Java program. Make sure the
class name matches the file name. Place this file in the `runtime/src/` directory and compile with
`javac -d build <classname>.java lang/promises/*.java`. Now `cd build/` and run `java <classname>`.

## Language syntax

```
# Types
Unit # like void
Int
Bool
Promise(Type) # promise of a value
Promise*(Type) # *owned* promise of a value

# Values
() : Unit
1 : Int
-5 : Int
true : Bool
false : Bool

# Functions
func <name>(<params>): <return> begin
  <code>
end

# e.g.

func chooseFirstOne(a: Int, b: Int): Int begin
  a # Automatically return the last value
end

# Main function
func main(): Unit begin
  # ...
end

# Calling functions
<name>(<args>)

# e.g.
f(1, true, p)

# Asynchronous execution
async <function call>

# e.g.
async f(1, true, p) # If p: Promise*(...) then this can hand ownership over and convert to p: Promise(...)

# Let bindings
let <name>: <type> = <value> in
<code>

# e.g.
let p: Bool = true in
p

# Sequences
<code>; # Sequence code with semi-colon
<code>  # No semi-colon after last expression

# Promises
let p: Promise*(Int) = promise Int in # Initialize promise (owned)
let q: Promise(Int) = p in # or unowned
p <- 6; # Write value to promise (only works with owned promises)
?q # Read value from promise

# Conditionals
if <boolean expr> then
  <expr>
else
  <expr>
end

# Loops
while <boolean expr> begin
  <expr>
end

for <name> = <initial value> to <final value> begin
  <expr>
end
```
