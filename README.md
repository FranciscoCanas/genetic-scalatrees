genetic-scalatrees
==================
- Francisco Canas

What be this?
=============
Classes that generate random source code trees in scala for fun genetic-programming mishaps. It's my work-in-progress project to learn about the powerful complexities of the Scala programming language.

What does this need?
====================
The Scala 2 language and a JVM capable of running it.

What's included?
================
Everything is a WIP and liable to be massively refactored, but so far:
- Tfunc: A wrapper class for functions.
- Stree: The scala source code tree class.
- Node and its extensions: Classes representing source code trees. The extensions are for function nodes, parameter nodes, and
  constant nodes.
- The treetest file: A very short demo that currently generates source code
  using a small subset of mathematical operations. 

What's needed?
==============
A lot more stuff. Like I said, this is a WIP for me to learn Scala and not some
end product. So for instance, I still need to implement:
- Mutation and crossbreeding of source trees.
- Generics and pattern-matching in the Tfunc function wrapper and random tree
  generation so as to more easily include functions using different return and parameter types.
- Forests of source code trees, evaluation functions, evolution, etc.

Wha?
====
See my genetic-pytrees repository for a slightly more fleshed out prototype of
this project that I wrote in python.


