# Scheme-Interpreter
This project implements a interpreter written in Racket. It includes a Read-Eval-Print Loop (REPL) that allows users to interactively evaluate expressions. The interpreter supports basic arithmetic, list manipulation, conditionals, variable definitions, and custom function creation, as well as special forms like define, if, cond, and lambda.

# Features
1. REPL (Read-Eval-Print Loop)
2. Arithmetic Operations
Supports basic arithmetic operations:
  Addition
  Multiplication
  Nested arithmetic

3. Variable Definitions
Users can define variables with define

4. Lists
Provides list operations:
  Create a list
  Access first element
  Access rest of the list
  Add an element

5. Conditionals

6. Custom Functions
Supports defining functions using lambda:

# Architecture
1. Tokenization
Functions tokenize user input to distinguish parentheses, numbers, identifiers, and special characters. Example tokens:

2. Parsing
Parses tokenized input into expressions for evaluation:

3. Evaluation
The evaluator processes the parsed expressions

4. Environment
The interpreter uses a global environment for variable bindings and built-in operations. It supports:

Arithmetic operators (+, -, *, /)
List functions (list, car, cdr, cons)

5.Error Handling
The interpreter raises descriptive errors for:

Undefined variables: (unknown-symbol) → "Error: Undefined variable: unknown-symbol"
Mismatched arguments: (define x) → "Error: Parameter and argument count mismatch"
