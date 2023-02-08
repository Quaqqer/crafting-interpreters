# HTreeLox

This is my Haskell implementation of the tree walker interpreter.
I must say I have not followed the book super closely, but rather done it my own way.
For instance I implemented the scanner and parser with my custom parser combinators.
I also made it a point to only use my own code for this project.
The exception being command line parsing because it's just not very interesting,
so I opted for [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative).

## Demo

This is a short demo, I am not done yet so more will come.

![Demo](./res/demo.svg)

## Scanning and parsing

Scanning and parsing in my implementation is done with my own
[parser combinators](https://en.wikipedia.org/wiki/Parser_combinator),
heavily inspired by [Megaparsec](https://github.com/mrkkrp/megaparsec).

I scan the source file for the tokens and then I parse the sequence of tokens, as
one might expect.

My parser combinators do not produce great error messages by any means in the
current state. Good error messages in a generalized library is pretty hard, but
I hope to resolve it soon.

## Interpreting

The interpretation is done in my Interpreter monad. The interpreter Monad is a
combination of IO, State and Either. IO could have been exchanged for Writer
since I don't really need the full IO, but I thought it was appropriate since
you may want to add some other IO operations. State probably speaks for itself,
Lox is an imperative programming language after all. Either is for handling
errors that may occur during runtime.

One special behaviour in my implementation
is that state will revert if a statement causes an error, so a statement that
makes an assignment and crashes afterwards will be reverted.

### Mutability

Since functions in Lox capture their environment we need to have multiple
references to variables and we need to be able to mutate them. We could simulate
this with a data store and store variables as handles to the values in the data
store. The problem with this is that we also need to count references to the
handles so that we know when to remove the values from the store. This is a lot
of work, so I opted to use [stm](https://hackage.haskell.org/package/stm), which
allows for mutable memory in haskell.

## General remarks

This has been a great exercise in both programming interpreters and programming
haskell. I have gotten a deeper understanding for some typeclasses such as
Monads ( _buzz buzz_ ) since I implemented them on my own and even the MonadIO
class (not that it was very hard).

I will probably continue working with similar things in my free time.
