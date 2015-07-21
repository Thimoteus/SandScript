# SandScript [![Build Status](https://travis-ci.org/Thimoteus/SandScript.svg?branch=master)](https://travis-ci.org/Thimoteus/SandScript) [![Dependency Status](https://gemnasium.com/Thimoteus/SandScript.svg)](https://gemnasium.com/Thimoteus/SandScript)

A lisp-like language that runs on node, written in Purescript (which is written in Haskell).

# Using

First make sure you have purescript (>= 0.7.0) and pulp (>= 4.0.2) installed, as well as node and npm.

Run `pulp dep install` and `npm install`, then `pulp run` to get a REPL.

Alternatively, you can run a file with `pulp run --run filename`.

## Building the interpreter

```bash
pulp build --optimise --to sandscript.js
sed -i '1s/^/#!\/usr\/bin\/env node\n/' sandscript.js
chmod +x sandscript.js
```

# Syntax

Everything is an [S-expression](https://en.wikipedia.org/wiki/S-expression). [Quoted lists](http://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp) are supported.

Defining variables is done with `def`, and bound variables can be rebound with `set`.

Defining functions is similar to defining a variable, for an example, see the `lang/Examples` folder.

## Highlighting

SandScript shares many names with Clojure, so setting your favorite text editor to highlight SandScript files as Clojure may provide a better experience.

# Semantics

## Data types

Primitive types come in the following flavors:

1. Atoms => 'atom
2. Strings => "hello world"
3. Bools => true, false
4. Ints => 2, ~2
5. Floats => 3.14, ~3.14
6. Fracs => 2/3, ~2/3
7. Complex => 1.0+0.0i, ~0.5+~3.14i

There are three types of collections: (linked) lists, [dotted lists](http://stackoverflow.com/questions/8358783/what-was-a-reason-to-introduce-dotted-pair-in-lisp) and arrays, all of which are heterogeneous.
