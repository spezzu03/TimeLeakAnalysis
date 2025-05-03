# Timing Leaks Analysis in GCL

## The project goal

#### "Can we use language-based security techniques and tools to detect timing leaks?"

The goal of this project is to see how far one can go when answering this question. Using formal methods, the scope of the program is to instrument input programs with a time variable, which tracks time according to the control flow and evaluation of expressions. Some assumptions were made, to keep the the level of abtraction high.

## The repository

**"src/Clock.fs"** takes care of instrumenting the AST with the time variable and updating it according to some rules that can be inferred from the function.

**"src/Verification.fs"** has the functions to generate the parts of an annotated program using self-composition.

**"src/ModularComposition.fs"** has the necessary functions to compose programs modularly, according to the definition from the paper.

**"src/Program.fs"** combines everything together, containing the logic for the tool application.

The program supplies a Hoare contract, such that non-interference can be verified by an external tool, called [Chip](https://team-checkr.github.io/chip)

## The language

The language grammar specification can be found in "src/Parser/Parser.fsy", and the tokens in "src/Parser/Lexer.fsl". It is reported here for convenience:

```
C  ::=  x := a  |  skip  |  C ; C  |  if GC fi  |  do GC od
GC ::=  b -> C  |  GC [] GC
a  ::=  n  |  x   |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)
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

To use the program, write a program in "src/Data/input.txt", or a file of your own. You will need a first line on the format "private: var?[,var]\*", where you specify the private variables. Then, in a new line, the GCL program. Some examples can be found on the "examples.txt" file.

To run the program, open a terminal in the the "src" folder, and run
`dotnet run [flags]`
The possible flags are:

- --mode: what shall be output. Can be one of {timed, modular, self}. Standard is self
- --src: source file. Standard is \"Data/input.txt\"
- --dest: destination file. Standard is \"Data/output.txt\"
- --sens: whether to use output-sensitive or -insensitive definition of ct. Can be one of {sensitive, insensitive}. Standard is insensitive.

**This program is part of a Bachelor Project**
