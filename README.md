# Hiet

Hiet is a Piet interpreter written in Haskell, and is my submission for the CMSC488B final project.

# Organization

Hiet's core logic consists of three modules, which are in `src/`: the interpreter (`Interpreter.hs`) for running Piet programs, image loader (`ImageLoader.hs`), and type definitions that the former two are dependent upon (`Types.hs`).  There is also a testing module `Tests.hs`, which consists of QuickCheck properties for Piet commands and execution invariants.

# Building + Running Hiet

To build Hiet, run `stack build`

To run Hiet, run `stack run -- p <path_to_image> -c <codel_size> -s`. To get the help menu run `stack run -- -h`.  The codel size is 1 by default, but may be overridden via the `-c` flag.  Supplying the `-s` flag tells the interpreter to print the final program state after termination.

# Testing

`test/Tests.hs` consists of QuickCheck properties for Piet commands and programs.  You can `stack ghci Tests.hs` and `runTests` to run all of the tests.  Some properties cause test cases to be discarded but everything should pass.  Tests the functionality of Piet commands minus essential duplicates (CharIn, CharOut, IntIn, IntOut), in which case just one is tested.  Also tests some invariants of Piet programs, e.g. the retries counter always is less than 8 (i.e. termination conditions are executed correctly), the program position never exceeds the grid size, and the input / output buffers never have more than one element in them.

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

```
$ stack run -- -p images/fizzbuzz.png 
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
$ stack run -- -p images/fac.png 
Input Int: 7
5040
```

```
$ stack run -- -p images/adder1.png 
nInput Int: 5
nInput Int: 5
5+5=10
```

```
$ stack run -- -p images/piet_pi.png 
31405
```

```
$ stack run -- -p images/hw1.gif -c 11
Hello, world!
```

With final state shown:
```
$ stack run -- -p images/piet_pi.png -s
31405
=====================Final State=====================
State {
  _stack = Stack [],
  _dp = DP {_dpdir = DPRight},
  _cc = CC {_ccdir = CCRight},
  _pos = (58,137),
  _cb = 1,
  _rctr = 8,
  _inbuf = [],
  _outbuf = []
}

```

Note: ghci disables buffering by default, so I had to add a `hFlush stdout` after every `putStr` to ensure consistent behavior between `stack run` and `ghci`.  However, this causes some weird behavior with printing sometimes.

# Disclaimer

Uses StateT but is still ugly :/
