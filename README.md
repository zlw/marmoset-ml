## Monkey Language but in OCaml 🐵 🐫
* [Writing Interpreter in Go](https://interpreterbook.com/) / [Writing Compiler in Go](https://compilerbook.com/)'s Monkey Language but in [OCaml](https://ocaml.org/)
* [Marmoset](https://en.wikipedia.org/wiki/Marmoset) is a small monkey, sometimes called Pug Monkey, and I do like pugs 🙈

## Installation

First you need to install OCaml, opam and dune:
* [official instruction](https://ocaml.org/docs/installing-ocaml)

Then you can clone this repository and run the following commands:

```sh
git clone git@github.com:zlw/marmoset.git
```

And then install the dependencies:
```sh
make install
```

## Build

### Dev
faster compilation, slower runtime performance
```sh
make build
```

### Release
slower compilation, faster runtime performance
```sh
make release
```

## Run tests

```sh
make unit
```

## Progress

- [x] Lexer
- [x] Parser
- [x] Evaluator
- [ ] Compiler

## Features

| Feature                | Interpreter | Compiler |
|------------------------|-------------|----------|
| Bindings               | ✅          | ❌       |
| Conditionals           | ✅          | ❌       |
| Strings                | ✅          | ❌       |
| Integers               | ✅          | ❌       |
| Arithmetic +-/*        | ✅          | ❌       |
| Arrays                 | ✅          | ❌       |
| Hashes                 | ✅          | ❌       |
| Functions              | ✅          | ❌       |
| First class functions  | ✅          | ❌       |
| Higher order functions | ✅          | ❌       |
| Closures               | ✅          | ❌       |
| Recursion              | ✅          | ❌       |
| Built-In Functions     | ✅          | ❌       |
| Floats                 | ❌          | ❌       |
| Macros                 | ❌          | ❌       |

## TODO

- [x] Cleanup pyramid of doom in `Parser`
    - [x] Use `Result.bind`
    - [x] Propagate parsing errors instead of crashing
- [ ] Add system tests
  - [ ] test runner
  - [ ] test cases (maybe reuse some from Crafting Interpreters?)

## Ideas

Monkey supports closures and first class functions. It would be interesting to add some functional programming features to it:
- [ ] immutability
- [ ] static typing with Hindley–Milner style type inference
- [ ] pattern matching

We those in place, it would be a JS-looking language with a ML core 🤔
