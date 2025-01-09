# Micro Lisp Interpreter in Racket

A minimal Scheme/Lisp-like interpreter written in [Racket](https://racket-lang.org/). This project demonstrates how to build a small Lisp dialect from scratch, complete with:

- A custom tokenizer and parser
- An environment for variable bindings (with lexical scoping)
- Support for special forms (e.g., `if`, `lambda`, `define`, `cond`, `begin`, logical operators)
- A simple Read-Eval-Print Loop (REPL)

## Table of Contents

- [Features](#features)
- [Project Structure](#project-structure)
- [Requirements](#requirements)
- [Usage](#usage)
  - [Running the Interpreter](#running-the-interpreter)
  - [Example Session](#example-session)
- [Extending the Interpreter](#extending-the-interpreter)
- [License](#license)

## Features

1. **Lexical Scoping**: Functions (`lambda`) capture the environment in which they’re defined, ensuring variables remain accessible inside the function’s body.
2. **Built-in Functions**: Includes standard arithmetic (`+`, `-`, `*`, `/`), list operations (`list`, `cons`, `car`, `cdr`), and comparisons (`>`, `<`, `>=`, `<=`, `=`).
3. **Special Forms**:
   - `define` for binding variables,
   - `lambda` for function creation,
   - `if` for conditional branching,
   - `cond` for multi-branch conditional,
   - `and` / `or` / `not` for logical operations,
   - `begin` for sequencing multiple expressions.
4. **Parser and Tokenizer**: Reads expressions from standard input, tokenizes them, and builds an internal list-based representation.

## Project Structure

If you keep everything in a single file (e.g., `micro-lisp.rkt`), your structure might be:



5.Error Handling
The interpreter raises descriptive errors for:

Undefined variables: (unknown-symbol) → "Error: Undefined variable: unknown-symbol"
Mismatched arguments: (define x) → "Error: Parameter and argument count mismatch"
