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


JuLox is also pretty easy to play with in the REPL. For example, if you want to see a pretty-printed lossy syntax tree for a file.

```julia
lossy_tree = read("lox_examples/recursive_fibonacci.lox", String) |> JuLox.Parse.parse_lox |> JuLox.LosslessTrees.build_tree |> JuLox.LossyTrees.to_lossy
```

```shell
     1:128    │[toplevel]
     1:72     │  [fun_decl_statement]
     5:7      │    :fib
     9:9      │    :n
    11:72     │    [block]
    13:38     │      [if_statement]
    20:25     │        [infix_operation]
    22:23     │          <=
    20:20     │          [variable]
    20:20     │            :n
    25:25     │          1.0
    27:36     │        [return_statement]
    34:35     │          [variable]
    35:35     │            :n
    37:70     │      [return_statement]
    46:69     │        [infix_operation]
    58:58     │          +
    46:56     │          [call]
    46:49     │            [variable]
    47:49     │              :fib
    51:55     │            [infix_operation]
    53:53     │              -
    51:51     │              [variable]
    51:51     │                :n
    55:55     │              2.0
    59:69     │          [call]
    59:62     │            [variable]
    60:62     │              :fib
    64:68     │            [infix_operation]
    66:66     │              -
    64:64     │              [variable]
    64:64     │                :n
    68:68     │              1.0
    73:128    │  [for_statement]
    80:89     │    [var_decl_statement]
    84:84     │      :i
    88:88     │      0.0
    73:128    │    [for_statement]
    90:96     │      [infix_operation]
    93:93     │        <
    90:91     │        [variable]
    91:91     │          :i
    95:96     │        20.0
    73:128    │      [for_statement]
   109:128    │        [block]
   111:126    │          [print_statement]
   119:125    │            [call]
   119:122    │              [variable]
   120:122    │                :fib
   124:124    │              [variable]
   124:124    │                :i
    98:107    │        [assignment]
    98:107    │          [assignment]
    99:99     │            :i
   102:107    │            [infix_operation]
   105:105    │              +
   102:103    │              [variable]
   103:103    │                :i
   107:107    │              1.0

```

## What's special

This implementation draws on a few ideas used (and nicely explained) in the lovely [JuliaSyntax.jl](https://github.com/JuliaLang/JuliaSyntax.jl) next-generation Julia frontend. In particular, we strive for looser coupling between tokenization, parsing, syntax tree creation, and analysis/interpreting.

The parser generates events, not trees. We've got two syntax trees, one lossless and the other lossy. 

## History

### 2023-05-27

- Tree-walk interpreter functioning!
- This repo published on GitHub under the MIT license
