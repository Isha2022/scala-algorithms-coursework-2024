# scala-algorithms-coursework-2024
# Scala Coursework Projects

This repository contains a collection of Scala assignments completed as part of a university module on functional programming and language implementation.  
All solution code is my own work; the tasks were based on templates provided by the course lecturer.

---

## Contents

Each subfolder contains a self-contained Scala file (or files) for a particular task:

### 1. `collatz-and-similarity/` (C1, C2)

- **C1 – Collatz utilities**  
  Implements several utilities for exploring the Collatz (3n + 1) conjecture, including computing step counts, finding maximum chains, detecting powers of two, identifying special (“hard”) numbers, and extracting the last odd value before the sequence collapses.

- **C2 – Text/code similarity**  
  Implements a simple text similarity tool: it tokenises strings into words, counts their occurrences, computes a dot-product–based overlap between word-frequency vectors, and uses that as a normalised similarity score (e.g. for basic plagiarism-style similarity detection).

---

### 2. `shunting-yard/` (C3a, C3b)

- **C3a – Basic Shunting Yard**  
  Implements the basic Shunting Yard algorithm to convert infix expressions with `+ - * /` and parentheses into postfix (RPN), and then evaluates them with a stack-based interpreter.

- **C3b – Shunting Yard with associativity and exponentiation**  
  Extends the Shunting Yard implementation to support operator associativity and exponentiation (`^`), correctly parsing and evaluating more complex arithmetic expressions.

---

### 3. `evil-wordle/` (M2)

- **M2 – Evil Wordle solver**  
  Implements an “evil” Wordle helper: it loads a list of valid secrets, scores guesses with Wordle-style feedback, repeatedly picks the remaining secrets that minimise the guess’s information gain, and ranks them by letter-frequency heuristics to choose the nastiest possible next answer.

---

### 4. `regex-derivatives/` (M3)

- **M3 – Regular expression engine**  
  Implements a regular-expression engine based on Brzozowski derivatives, with smart simplification, that can build regex ASTs using lightweight Scala syntax, compute derivatives and sizes, and efficiently decide whether a string matches even “evil” regexes like `(a*)* b`.

---

### 5. `shogun-board-game/` (M4)

- **M4 – Shogun-style board game logic**  
  Models a Shogun-style chess variant: defines pieces, boards, and directional moves with energy-based movement, can evaluate all reachable positions for a piece, and determines which opposing pieces are currently under attack (with further tasks for protection/legality logic).

---

### 6. `brainfuck-interpreters/` (M5a, M5b)

- **M5a – Brainfuck interpreter**  
  Implements a basic Brainfuck interpreter in Scala, with sparse memory, bracket matching via recursive jumps, input/output, and a small code generator to emit Brainfuck that prints a given message.

- **M5b – Optimising Brainfuck interpreter / “compiler-style”**  
  Adds an optimising Brainfuck interpreter that builds jump tables for loops, strips dead code, recognises clear-cell loops, compresses repeated commands, and executes Brainfuck programs much faster than the naive interpreter.
