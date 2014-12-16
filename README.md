Tiger Compiler
=====

Tiger Compiler from "Modern Compiler Implementation in ML" by Andrew
W. Appel


### Introduction

This is a Tiger compiler with all modules completed until Chapter 12
in the book. Basically I followed all the instructions from the book
without much alternation. I also tried to
adhere to the functional style as much as possible, that means,
minimize the usage of mutable data structures.


### Requirement

So far the compiler works with either mac or linux. Besides that,
you need to have Standard ML and spim installed. Both of them can be
fetched via homebrew.

Standard ML is obviously need to compile the programs. Since the
compiler generates spim code, you'll need the
[SPIM simulator](http://spimsimulator.sourceforge.net/) to actually
run the executables.

### Build
To build the compiler, simply run

```
make main
```

This will build the image using `sources.cm`. Note that it currently
doesn't support windows.

### Compile

Given a tiger source program `foo.tig`, to compile it, you'll need to first copy
the file to the project directory, and run

```
make foo
```

This will compile the file, append `runtime.s` to it, and generate a
output SPIM program called `foo.spim`. Then to execute the program,
run

```
spim -file foo.spim
```

### Future Work

The second half of the book covers a lot of interesting stuff, and I'm
thinking about extending the compile with the following features:

* Garbage Collection
* OO Features
* Functional Features
* Polymorphism and Type Inference
* Optimizations
