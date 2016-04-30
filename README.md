# SandScript [![Build Status](https://travis-ci.org/Thimoteus/SandScript.svg?branch=master)](https://travis-ci.org/Thimoteus/SandScript) [![Dependency Status](https://gemnasium.com/Thimoteus/SandScript.svg)](https://gemnasium.com/Thimoteus/SandScript)

A lisp-like language that runs on node, written in Purescript (which is written in Haskell).

# Using

First make sure you have purescript (>= 0.8.5) and pulp (>= 8.1.0) installed, as well as node, npm and bower.

Run `bower install` then `pulp run --interactive` to get a REPL.

Alternatively, you can interpret a file with `pulp run $(cat <filename>)`.

## Building the interpreter

```bash
pulp build --optimise --to ssc.js
sed -i '1s/^/#!\/usr\/bin\/env node\n/' ssc.js
chmod +x ssc.js
```

# Syntax

Everything is an [S-expression](https://en.wikipedia.org/wiki/S-expression). [Quoted lists](http://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp) are supported.

Values are immutable.

Defining variables is done with `def`: `(def x 3)`

Defining functions is similar:

```lisp
(def (f x y) (+ x y)

(def (compose f g)
  (lambda (x) (f (g x))))
```

# Semantics

## Data types

Primitive types come in the following flavors:

1. Atoms => 'atom
2. Strings => "hello world"
3. Bools => True, False
4. Ints => 2, -2

There's one type of collection, a linked list. List literals can be made as follows: `'(1 True "string")`.

# Hacking

While based on [Write Yourself A Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),
there are quite a few differences:

1. Instead of using Refs to store the environment, the environment is stored in a StateT transformer monad.
2. Instead of using an association list as the environment, StrMaps are used
3. No support for dotted lists
4. "IO primitives" are handled by the REPL as directives instead of given as language primitives.

In particular, the base monad for the transformer stack is left polymorphic:
The REPL uses Aff to deal with user input, and the source-code file interpreter uses Eff.
In principle this means one could use, for example, the Trampoline monad at the base to
handle stack overflows in the language (which were a serious problem in a previous version),
though I haven't tested this yet.
