# HaskSat

This project contains an implementation of a haskell-based CDCL algorithm. The repository is a cabal package, so it can be built using cabal commands.

For executable `dimacs`, input and output is done via `stdin`/`stdout`, and uses the DIMACS satisfiability format. The program only accepts formulae in conjunctive normal form.

A second executable, `sudoku`, is included, which solves sudoku puzzles. The program reads a line containin 81 periods or digits (the initial contents of the puzzle in row-major order), and outputs a 9 by 9 matrix of digits, where the digits on a line aren't separated by whitespace. The solver assumes that a solution exists.
