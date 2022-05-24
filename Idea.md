# Idea for a Piet to x86_64 compiler

1. Maintain the following registers: 

* `rax`: direction pointer state
* `rbx`: codel chooser state
* `rcx`: address of current color block (label address should be known at compile-time)
* `r8`: color of previous color block
* `r9`: color of current color block
* `r10`: number of codels in a color block.
* `r11`: Piet stack depth (so we can add back the correct offset to align the stack after upon termination, if termination occurs).
* `r12`: retries counter (if we hit 8, then terminate the program)
* `r13`: old label address (we need this to determine whether or not to increment the retries counter)

Everything else can be just pushed / popped from the stack, though I suspect handling edge cases such as there not being enough operands on the stack will be pretty annoying.

Notes:

* The color of each block (discarding white and black) can be represented by some value in Zmod(18), so that their differences, which encode the command to be executed, are also in that range.  
* We can compile an auxiliary function that decodes the command based on `r9 - r8` and chooses which command to call.

# Execution

Like other languages, we can model Piet control flow like a directed graph: each vertex represents a color block and each edge represents a transition between color blocks, which encodes the command to be executed.  Each color block has at most four adjacencies; we can randomly generate a label name for each vertex and store it in a record like so:

```
haskell
data Vertex = Vertex {
    name :: String
    positions :: [Position]
    adjacencies :: [Vertex]
}
```

