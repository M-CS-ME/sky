**Some old stuff.** Most of these are either broken or incomplete attempts at
this projects, not really worth looking at.

<!--
Sky: A λ-Calculus Interpreter
=============================

Sky is an interpreter for the untyped λ-calculus, written in python for my own amusement.

It uses β-reduction and an evaluator similar to: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1

Updates
=======

--------------------------------------------------------------------------------
06 Aug 2021:
This is going for a rewrite, too many bugs that idk how to fix. Issues include:
1. Transforming nested function application into currying
2. Environment model

I'm also probably going to try to learn a bit more about interpretation before I
start the rewrite.

Rewrite might be in haskell or just in python.
--------------------------------------------------------------------------------

Goals
=====

1. Environment for Variables (typing λ-expressions is hard)
2. α-equivalence (either internally or callable from the interpreter)
3. Module System
4. Better syntax
5. IO (preferably using Monads, but idk if that's possible in an untyped language)
6. Data structures (pairs, trees, lists, etc)

Sky is considered complete when I can make a merge sort with it. Although I might choose to continue it.

Tutorial
========

If you are not familiar with the λ-calculus, checkout Peter Selinger's lecture notes: https://arxiv.org/abs/0804.3434 . The first few chapters should be enough to get you started.

The syntax is as follows (for now):

      +----------+---------------+
      | λ-expr   | Sky syntax    |
      +----------+---------------+
      | λxy.x    | (\ (x y) x)   |
      +----------+---------------+
      | MN       | (M N)         |
      +----------+---------------+

You may have realized that this is similar to scheme's syntax (parsing s-exprs is MUCH easier than parsing anything else), and that is true, the only difference is that lambdas are represented with either a "\" or a "λ" as opposed to "lambda"
-->
