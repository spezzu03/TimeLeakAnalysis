# Timing Leaks Analysis in GCL

## The project goal

#### "Can we use static program analysis techniques and tools to detect timing leaks?"

The goal of this project is to see how far one can go when answering this question. Using program analysis techniques, the scope of the program is to instrument input programs with a time variable, which tracks time according to the control flow and evaluation of expressions. Some assumptions were made, to keep the the level of abtraction high.

## The repository

The main characters of this repo are **"src/Clock.fs"** and **"src/Program.fs"**. The former takes care of instrumenting the AST with the time variable and updating it according to some rules that can be inferred from the function. The latter, while joining everything together, supplies a Hoare contract, such that non-interference can be verified by an external tool, called [Chip](https://chip-pv.netlify.app).

The language grammar specification can be found in "src/Parser/Parser.fsy", and the tokens in "src/Parser/Lexer.fsl". It is reported here for convenience:

```
C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od
GC ::=  b -> C  |  GC [] GC
a  ::=  n  |  x  |  A[a]  |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)
b  ::=  true  |  false  |  b & b  |  b | b  |  b && b  |  b || b  |  ! b
     |  a = a  |  a != a  |  a > a  |  a >= a  |  a < a  |  a <= a  |  (b)
```

A loop invariant can also optionally be included, as

```
C::= do[b] GC od
```

## Executing the program

#### Requirements

In order to run the program, you need

- **.NET 9.0**
- **FsLexYacc 11.3**

#### Running the program

To use the program, write a program in "src/Data/input.txt". You will need a first line on the format "private: var?[,var]\*", where you specify the private variables. Then, in a new line, the GCL program.

To run the program, open a terminal in the the "src" folder, and run
`dotnet run`

The product program is to be found in "src/Data/output.txt"

**This program is part of a Bachelor Project**
