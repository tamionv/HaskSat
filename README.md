# HaskSat

This project contains an implementation of a haskell-based CDCL algorithm. The repository is a cabal package, so it can be built using cabal commands. Input and output is done via `stdin`/`stdout`, and uses the DIMACS satisfiability format. The program only accepts formulae in conjunctive normal form.

A second executable, `sudoku`, is included, which solves sudoku puzzles. The program reads a 9 by 9 matrix of digits, or `.`, and outputs a 9 by 9 matrix of digits, where the digits on a line aren't separated by whitespace.
