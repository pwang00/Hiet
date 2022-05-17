Ok this idea _might_ work but not sure: 

1. Maintain 5 registers: one for direction pointer state, one for codel chooser state, one to store the address of the next color block to move to (label address should be known at compile-time), one to store the difference in color between color blocks, and one to store the Piet stack depth (so we can add back the correct offset to align the stack after upon termination, if termination occurs).

At compile time: 

Maintain some structure to hold the Compute the borders of all color blocks and their adjacencies; between each border you can compute the difference in color which determines the command to be executed
