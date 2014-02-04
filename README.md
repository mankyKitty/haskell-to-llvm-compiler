# Haskell : LLVM Jit

This is an implementation of a Jit targetting LLVM, built using Haskell and [this tutorial](http://www.stephendiehl.com/llvm) as a guide.

My implementation is built on OSX Mavericks with LLVM 3.4 so any differences in configuration and implementation from the original tutorial may be due to that. Also I don't really expect anyone but me to ever look at or use this so don't expect much help if you clone it and it doesn't work for you.

I will also probably be hacking on as part of other projects as it complements the tutorial for `Write Yourself a Scheme in 48 Hours` quite nicely.

So far I've it configured so that all I need to do is the following to make it build, maybe the gods of Haskell allow me to continue keeping it this simple after all of the dicking around I had to get it to work so far.

```
$ cabal configure
$ cabal install --only-dependencies
$ cabal compile
```