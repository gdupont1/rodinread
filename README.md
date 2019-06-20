# `rodinread`
Reading/Transformation tool for dealing with Rodin files.

by G. Dupont

# Introduction

Event-B [http://www.event-b.org/] is a correct-by-construction formal method supported by a tool called **Rodin**.

Rodin handles multiple types of files that represent Event-B models, but handles them using XML. As such, they are not very readable for normal human beings and it is quite troublesome to export an interesting visual representation of those files.

This project's aim is to provide a simple-yet-complete tool to enable Rodin model pretty-printing.

The script supports the pretty-printing of machines, contexts, theory and even proof obligation files. It is written in Haskell (managed through Cabal).


# License

This work is being published under MIT License (see LICENSE file).

# Installation

## Cabal

The whole project is a Cabal executable project, so it is easy to use Cabal to build and install the script:
```
cabal configure
cabal build
```

And then `cabal install`, or simply manually take out the executable in `dist/build/rodinread/`.


## Standard GHC

GHC (Glasgow's Haskell Compiler) can be used directly to compile the project. However, as the project has multiple dependencies this is really not recommended.

In any way, for the record, here is the list of dependencies that need to be available for the project to build correctly:
 - **base >= 4.10**
 - **split >= 0.2**
 - **containers >= 0.5**
 - **xml >= 1.3**
 - **transformers >= 0.5**

In theory any version **should** work for all of these dependencies (provided they are the same major version).

Once every dependencies have been installed, simply run:
```ghc Main.hs```

This will generate a `Main` executable (it is recommended to rename it `rodinread` as it is more explicit).


# Usage

`rodinread -h` yields a basic usage of the script.


# Notes

The project is still in beta (especially regarding some advanced aprts such as the substitution mechanism) and it lacks comments...



