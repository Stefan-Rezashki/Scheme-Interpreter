# Micro Lisp Interpreter in Racket (Single-File Edition)

A minimal Scheme/Lisp-like interpreter written in [Racket](https://racket-lang.org/). This project demonstrates how to build a small Lisp dialect from scratch, complete with:

- A custom tokenizer and parser
- An environment for variable bindings (with lexical scoping)
- Support for special forms (e.g., `if`, `lambda`, `define`, `cond`, `begin`, logical operators)
- A simple Read-Eval-Print Loop (REPL)

## Table of Contents

- [Features](#features)
- [File Structure](#file-structure)
- [Requirements](#requirements)
- [Usage](#usage)
  - [Running the Interpreter](#running-the-interpreter)

## Features

1. **Lexical Scoping**: Functions (`lambda`) capture the environment in which they’re defined, ensuring variables remain accessible inside the function’s body.
2. **Built-in Functions**: Includes standard arithmetic (`+`, `-`, `*`, `/`), list operations (`list`, `cons`, `car`, `cdr`), and comparisons (`>`, `<`, `>=`, `<=`, `=`).
3. **Special Forms**:
   - `define` for binding variables,
   - `lambda` for function creation,
   - `if` for conditional branching,
   - `cond` for multi-branch conditionals,
   - `and` / `or` / `not` for logical operations,
   - `begin` for sequencing multiple expressions.
4. **Parser and Tokenizer**: Reads expressions from standard input, tokenizes them, and builds an internal list-based representation.

## File Structure

This repository contains a single file that implements the interpreter:

- **Scheme-Interpreter.rkt**: Contains all the core logic—environments, evaluation, parsing, special forms, and the REPL.

## Requirements

- **Racket** (version 8.x or later recommended). Download from [racket-lang.org](https://racket-lang.org/download/).

## Usage

### Running the Interpreter

1. **Clone the repository**:
   ```bash
   git clone https://github.com/your-username/micro-lisp.git
   cd micro-lisp
2.Run the file:
   ```bash
      racket micro-lisp.rkt
   

