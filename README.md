# JuLox

This repo contains the implementation of the Lox programming language from the book [Crafting Interpreters](https://craftinginterpreters.com).

## Installation

1. Get Julia 1.9+ (earlier versions might also work)
2. Checkout the code `git clone ...`
3. Instantiate this package's environment (inside the Julia package management REPL mode, that would be something like `activate /path/to/JuLox` and then `instantiate`)

## Usage Examples

You can run files like this: `./julox lox_examples/recursive_fibonacci.lox`

Enter the repl like this: `./julox`

You can have JuLox pretty-print all the internal intermediate state (tokens, parser events, syntax trees, variable resolution scope info) by adding the `--verbose` flag, e.g. `./julox --verbose`, too.

## What's special

This implementation draws on a few ideas used (and nicely explained) in the lovely [JuliaSyntax.jl](https://github.com/JuliaLang/JuliaSyntax.jl) next-generation Julia frontend. In particular, we strive for looser coupling between tokenization, parsing, syntax tree creation, and analysis/interpreting.

The parser generates events, not trees. We've got two syntax trees, one lossless and the other lossy. 

## History

### 2023-05-27

- Tree-walk interpreter functioning!
- This repo published on GitHubu under the MIT license
