# Parser Extra

This library provide useful combinators for working with the [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.0.0/) library such as parsing expression between parentheses, braces, brackets or any other symbols.

[![Build Status](https://travis-ci.org/Punie/elm-parser-extras.svg?branch=master)](https://travis-ci.org/Punie/elm-parser-extras)

## Why?

I wanted to try out the shiny new upcoming (now released) version 0.19 of Elm.

My little side-project for doing so was a dead-simple interpreter for a little [_Simply Typed Lambda Calculus_](https://github.com/Punie/elm-stlc).

Over time, I found myself needing a bunch of little helpers and combinators for writing the parser. I figured _"Why not put them in a `Parser.Extras` package of their own?"_ (which they were already, albeit in the `src` directory of my small project).

I also wanted to simplify the construction of expression parsers with different types of operators (infix, prefix, postfix) with different associativity rules and precedence. For this, I reached for the Haskell library `Text.Parsec.Expr` for inspiration and added the `Parser.Expression` module to my little library.

## Roadmap

- [x] Add some tests
- [x] Add a changelog
- [ ] Add contributing guidelines
- [ ] Support the new version `1.1.0` of [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/) that exposed the `Parser.Advanced` module (see #3)
- [ ] Gather feedback and add some more useful combinators to `Parser.Extras`

## Licence

[BSD-3-Clause](LICENSE) :copyright: Hugo Saracino
