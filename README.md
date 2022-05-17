# Hiet

Hiet is a Piet interpreter written in Haskell, and is my submission for the CMSC488B final project.

# Organization

Hiet's core logic consists of three modules, which are in `src/`: the interpreter (`Interpreter.hs`) for running Piet programs, image loader (`ImageLoader.hs`), and type definitions that the former two are dependent upon (`Types.hs`).  There is also a testing module `Tests.hs`, which consists of QuickCheck properties for Piet commands and execution invariants.

# Building + Running Hiet

To build Hiet, run `stack build`

To run Hiet, run `stack run -- p <path_to_image> -c <codel_size> -s`. To get the help menu run `stack run -- -h`.  The codel size is 1 by default, but may be overridden via the `-c` flag.  Supplying the `-s` flag tells the interpreter to print the final program state after termination.

# Testing

`Tests.hs` consists of QuickCheck properties for Piet commands and programs. 

# Examples + What to Expect

There's a few programs in the `images` directory:
    * Hello world: `hw1.gif` (codel size 11)
    * Hello world 2: `hw-small.gif` (codel size 1)
    * Factorial: `fac.png` (codel size 1)
    * Adder: `adder1.png` (codel size 1)
    * Print out digits of Pi: `piet_pi.png` (codel size 1)
    * Euclidean algorithm: `euclid1.png` (codel size 1)
    * FizzBuzz: `fizzbuzz.png` (codel size 1)

Some of their outputs are below:

```stack run -- -p images/fizzbuzz.png 
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
```

```
stack run -- -p images/fac.png 
Input Int: 7
5040
```

```
stack run -- -p images/adder1.png 
nInput Int: 5
nInput Int: 5
5+5=10
```

```
stack run -- -p images/piet_pi.png 
31405
```

```
stack run -- -p images/hw1.gif -c 11
Hello, world!
```

With final state shown:
```
stack run -- -p images/piet_pi.png -s
31405
=====================Final State=====================
State {_stack = Stack [], _dp = DP {_dpdir = DPRight}, _cc = CC {_ccdir = CCRight}, _pos = (58,137), _cb = 1, _rctr = 8, _inbuf = [], _outbuf = []}
```

Note: ghci disables buffering by default, so I had to add a `hFlush stdout` after every `putStr` to ensure consistent behavior between `stack run` and `ghci`.  However, this causes some weird behavior with printing sometimes.

# Disclaimer

If you are any good at Haskell, you may be wondering: "Why didn't you do it this way?" where "it" could refer to using constructs like the `State` monad or `StateT` monad transformer instead of explicitly modifying and passing around state via the ugly `state@{_field = newVal}` syntax.

Truth be told, I'm not any good at Haskell, and you're probably right.  I also wanted to do the above but I was pressed on time and ended up spending a lot of it debugging my interpreter, which would fail on some images. Also, unfortunately deadlines exist, so I chose to submit a uglier looking working implementation over a nicer looking broken one.  I will, however, refactor this code once the semester is over just for sake of completeness.  