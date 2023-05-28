# JuLox?

This directory contains the implementation of the `Fractal.Lox` submodule, which is a tree-walk interpreter for the Lox programming language from the book [Crafting Interpreters](https://craftinginterpreters.com). The point of this submodule was to test out ideas of how to implement a parser in Julia on a language that is simpler than Fractal (Lox is desiged for teaching simplicity, after all!).

## Ideas tested / expected learnings

- Borrow design and code from [`JuliaSyntax.jl`](https://github.com/JuliaLang/JuliaSyntax.jl/tree/main/src) and [`Tokenize.jl`](https://github.com/JuliaLang/Tokenize.jl)
  - Use of the special `Kind` type for token types
  - Keeping track of source position for error handling
  - Tokenization converts text to tokens
  - Parsing converts tokens to "events"
  - Events can be used to construct a "Green tree", which is a lossless syntax tree (lossless means we can convert this back to the exact original text!)
    - This helps with nice error messages
  - Green trees can be converted to abstract syntax trees
    - These can be interpreted
- Learn to write a decent REPL with some color and `ctrl-D` quitting in Julia
- Try out organizing a langauge interpreter into Julia submodules
- [Stretch goal] go beyond a tree-walk interpreter and transpile Lox to Julia!