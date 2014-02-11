# Haskell : LLVM Jit

LLVM Targetting JIT written in Haskell for Great Good

This is an implementation of a Jit targetting LLVM, built using Haskell and [this tutorial](http://www.stephendiehl.com/llvm) as a guide.

My implementation is built on OSX Mavericks with LLVM 3.4 so any differences in configuration and implementation from the original tutorial may be due to that. Also I don't really expect anyone but me to ever look at or use this so don't expect much help if you clone it and it doesn't work for you.

I will also probably be hacking on as part of other projects as it complements the tutorial for `Write Yourself a Scheme in 48 Hours` quite nicely.

## Building

So far I've it configured so that all I need to do is the following to make it build, maybe the gods of Haskell allow me to continue keeping it this simple after all of the dicking around I had to get it to work so far.

```
$ cabal configure
$ cabal install --only-dependencies
$ make
```
To clean up just ```$ make clean```. 

## Compiling to Native/Target Architecture

It is also possible to compile the source code that is produced by the
```kaleidoscope``` executable to a native app. This is a bit of a faff
at the moment how it should be relatively clear once you see it in
action.

__This process is broken for including multiple files at the moment as
the internal process for compiling to LLVM IR code has some left over
debugging niceties that can cause problems.__

### Single File

The steps for compiling a single file from LLVM IR code to a native
(or otherwise) executable are as follows:

Assuming a starting code file:```foo.kal```, and LLVM version 3.4 OSX.
```
$ ./kaleidoscope foo.kal > foo.ll # produces the LLVM IR code.
$ llc-3.4 foo.ll -o foo.bc
$ cc foo.bc -o foo.native
```
Provided you've no errors in your code, then this should produce an
executable file call ```foo.native``` that you can run thus:
```
$ ./foo.native
```

### Multiple Files

There is an extra step involved when you have several files that you
wish to combine into a single executable. Currently this little (iddy
biddy) compiler has no support for working across multiple files or
anything related to imports. It's coming but currently it's just not.

To handle several files you must link them into a single LLVM code
file, like so:
```
$ llvm-link-3.4 file1.ll file2.ll file3.ll -o all_code.ll
```
This file is then compiled to native using the single file process above.

### Including Extern Files

As we support using shared library functions written in ```C``` it
would be a bit weird if we could include them in out compiled native
application. However, at the moment you need access to the raw code
for the external code you're including, since we have to compile it to
LLVM IR code prior to being included. On OSX using Clang this is the
command to create the LLVM code:
```
$ clang -emit-llvm file2.c -c -o file2.ll
```
The resulting file has to be link to your application and then finally
the single file is compiled using the single file instructions.

### Target different architectures

Through the magic of LLVM, the bytecode we produce can be tailored to
suit different systems. The choice is made when you're compiling the
LLVM IR to LLVM Bytecode, an additional flag lets you create the
required LLVM Bytecode to the native assembly for a target
architecture:
```
$ llc-3.4 -march=arm file1.bc -o file1.s
$ cc file1.s -o file1.arm
```
