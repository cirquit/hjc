# hjc
The Haskell MiniJava Compiler build at the LMU course "Compiler Construction"

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
String
String[]
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
