# hjc
The Haskell MiniJava Compiler build at the LMU course "Compiler Construction"


### To use the C-- Backend

Compile to `name.tree` with a configurable flag (missing).
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



### Development

This project is using `stack-1.3.2`.

### Styleguide

Using `hindent-5.2.1`. Install with `stack install --resolver nightly-2016-10-17 hindent`. 


### Special Syntax

* definition + declaration
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

* additional types allowed
```
void
```

### Testing

* Run tests
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
