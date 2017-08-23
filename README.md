# hjc
The Haskell MiniJava Compiler build at the LMU course "Compiler Construction"

## How to use

```bash
> git clone https://github.com/cirquit/hjc
> cd hjc
> stack build && stack install
> hjc --help
hjc - A MiniJava Compiler in Haskell

Usage: hjc javaFile [--showAst] [-t|--showTime] [--canonCmm] [-c|--compileToCmm]
           [-w|--compileToX86] [-g|--createIFGraph] [--javaOutputDir javaDir]
           [--cmmOutputDir cmmDir] [--x86OutputDir x86Dir]
           [--cfOutputDir graphDir]
           [-e|--typeErrLvl [AllErrors|Silently|FirstError]]
           [-x|--compileToAllocatedX86] [--showJava] [-s|--inSequence]

Available options:
  --showAst                show Ast
  -t,--showTime            show Time
  --canonCmm               canonize Cmm
  -c,--compileToCmm        compile to Cmm
  -w,--compileToX86        compile to X86 Code
  -g,--createIFGraph       create Control Flow Graph
  --javaOutputDir javaDir  directory of java files
  --cmmOutputDir cmmDir    directory of cmm files
  --x86OutputDir x86Dir    directory of x86 assembly files
  --cfOutputDir graphDir   directory of control flow graph
  -e,--typeErrLvl [AllErrors|Silently|FirstError]
                           defines which level of errors should bee shown
  -x,--compileToAllocatedX86
                           compile to allocated X86 Code
  --showJava               show the recreated java output
  -s,--inSequence          run the computation sequential
  -h,--help                Show this help text

```

## Features

* definition + declaration is allowed
```java
int x = 1;
```

* if and else may be in a single line without braces
```java
if (boolean_value) do_something();
else do_something_else();
```

* parens in expressions allowed
```java
(1 + 2) * 3  // returns 6
``` 

* the register allocation is in parallel, by sharing the fresh variables in a an `IORef`. This is on by default and can be removed by the flag `-n`

* we support multiple level instruction removal by not used nodes in the graph and `mov`'s to unused temps

* we initialize every variable with `0` by default, so code like this is allowed
```java
int x = 2;
int y;
int z = y + x; // 2
```

* we created a simple DSL for working with the x86 assembly code, which supports comments for intermediate representation
```haskell
...
     DIV_C   -> do 
         mov eax op1  # "moving first arg to eax for idiv"
         t <- nextTempO 
         mov t op2
         cdq
         idiv t
         mov op1 eax  # "move idiv result to initial destination"
...
```

* we have a nice command line interface, which is extendable and can configure almost every switch in this compiler

* we've written our own parser with `megaparsec` and could therefore create nice error messages (unfortunately, colors are not visible here)

```bash
> hjc examples/Arg.java
>> Starting to lex examples/Arg.java
>> examples/Arg.java:8:18: error:

  unexpected tokens: [Tokens ('{' :| "")]
    expected tokens: [Tokens ('(' :| ""),Tokens ('_' :| ""),Label ('a' :| "lphanumeric character")]

...
       public int go{int i} {
...
                   ^^^
  failed parsing.
>> Finished in: 120.0ms
--------------------------------------------------------------------------------
```

* the same goes for the typechecking (with different error levels, configurable via flags)

```bash
> hjc examples/Arg.java
>> Starting to lex examples/Arg.java
>> Typechecking failed: 

        Terminating with 3 typeerrors.

   #1 class A : method go:
        identifier "j" is undefined and thus has no type

   #2 class A : method go:
        type  "Object" does not match expected types "int" in expression:

         j

   #3 class A : method go:
        operator  /  is not defined for type "Object", allowed types are "int"
```

* we also have a full testsuite on every automatically testable property of the steps in between

#### How to compile to binary and run

Compile with `hjc <filename>.java`. This will create a `x86output` folder for you, then you have to link it yourself. In this project structure, the folder already has a `Makefile` which can be used by `make file=<filename>-allocated.s`.

#### How to use the C-- Backend

Compile to `name.tree` with a configurable flag `-c` and `--canonCmm`.
Install the tree2c tool:

```
> git clone https://gitlab.cip.ifi.lmu.de/schoepp/Compilerbau17
> cd src/tree2c
> stack build
> stack install
```

Check if you have `libc6-dev-i386` or `g++-multilib` or `libc6-dev-i386` package installed.
In my case, this only worked with `clang-3.8`.

```
> cd cmm-output
> tree2c <name>-cmm.tree > <name>.c
> gcc -m32 <name>.c runtime.c -o <name>
> ./name
```

Otherwise, in the `cmm-output` folder, there is a prepared `Makefile`, which can be run by `make file=Add-canonized.tree`.

### Graph visualization

Install the graphviz program - `sudo apt-get install graphviz`.
Set the config flag `--createIFGraph`.
To show the graph as PostScript file:

```bash
> cd if-graph-output
> dot -Tps ProgramName.dot -o output.ps
```

Otherwise, in the `if-graph-out` folder, there is a prepared `Makefile`, which can be run by `make file=Arg.dot`.


### Development

This project is using `stack-1.3.2`.

### Styleguide

Using `hindent-5.2.1`. Install with `stack install --resolver nightly-2016-10-17 hindent`. 

### Testing

* Run tests (in the main folder of the repository)
```
stack test
```

* Run a specific test file i.e.
```
stack test --test-arguments "-m "TypeCheck/TypeCheck""
```

* Fail fast
```
stack test --test-arguments "--fail-fast"
```

* Run a specific test file, fail fast and filewatch
```
stack test --file-watch --test-arguments "-m "TypeCheck/TypeCheck" --fail-fast"
```

* Errors
    * `111`: IndexOutOfBounds

* Debug with `gdb`

```bash
> gdb ./testprog

>>> tui enable  -- enables a terminal ui
>>> refresh     -- redraws the tui

>>> b Lmain     -- set breakpoint at function Lmain
>>> n           -- next
>>> print var   -- shows var
>>> backtrace   -- shows stacktrace
>>> quit
```
