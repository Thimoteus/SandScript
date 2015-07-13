# SandScript [![Build Status](https://travis-ci.org/Thimoteus/SandScript.svg?branch=master)](https://travis-ci.org/Thimoteus/SandScript) [![Dependency Status](https://gemnasium.com/Thimoteus/SandScript.svg)](https://gemnasium.com/Thimoteus/SandScript)

A lisp-like language that compiles to JavaScript, written in Purescript (which is written in Haskell).

# Using

First make sure you have purescript (>= 0.7.0) and pulp (>= 4.0.2) installed.

Run `pulp dep install`, then `pulp run` to get a REPL.

# Syntax

Everything is an [S-expression](https://en.wikipedia.org/wiki/S-expression). [Quoted lists](http://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp) are supported.

Defining variables is done with `def`, and bound variables can be rebound with `set`.

Defining functions is similar to defining a variable, for an example, see the section on rational number arithmetic below.

# Semantics

## Data types

Primitive types come in four flavors: atoms ('atom), strings ("hello world"), bools (#t, #f) and natural numbers (0, 1, 2, ... ). There are two types of collections, lists and [dotted lists](http://stackoverflow.com/questions/8358783/what-was-a-reason-to-introduce-dotted-pair-in-lisp), both of which are heterogeneous.



# Questions That Might Be Asked Frequently

### Why is there no support for integers?

You can get integers, but for those smaller than 0 you'll need to write `(- 0 n)` instead of `-n`. For example:
```
> (- 0 2)
-2
```

### What about rationals?

Rationals are just pairs of integers. If you want to use them you'll have to define arithmetic operators according to the axioms of a [field](https://en.wikipedia.org/wiki/Field_%28mathematics%29#First_example:_rational_numbers). 

For example:

```clojure
(def (plus x1 y1 x2 y2) (cons (+ (* x1 y2) (* x2 y1)) (* y1 y2)))
(def (times x1 y1 x2 y2) (cons (* x1 x2) (* y1 y2)))
(def (neg x y) (cons (- 0 x) y))
```

etc.

### Are you planning on adding these features in the future?

No. I'm sure someone out there has written "Syntactic sugar considered harmful" so refer to that, if it exists.
