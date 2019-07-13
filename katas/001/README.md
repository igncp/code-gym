# 001

## Problem

Write a program that outputs all possibilities to put + or - or nothing between the numbers 1,2,…,9 (in this order) such that the result is 100. For example 1 + 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100.

## Initial Thinking

- Try and understand: https://stackoverflow.com/a/4633515/3244654
    - Recursive permutation
- It can be considered as a combinatory problem where every separation item between the 9 digits is: `'+'`, or `'-'`, or `''`
- Will try brute-force approach

## Solution

- Implemented in TypeScript
- Verify: `npm start`
- Used a recursive approach where it builds the combinations of operations and
  computes each result when finished
- Followed a TDD workflow using `assert` module and commenting checks when necessaru
- Possible optimizations:
    - Better data structure for the operations array
    - Stop branch when result too high with remaining numbers

## Sources

- https://adriann.github.io/programming_problems.html

    - Intermediate, number: 1
