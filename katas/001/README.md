# 001

## Problem

Write a program that outputs all possibilities to put + or - or nothing between the numbers 1,2,…,9 (in this order) such that the result is 100. For example 1 + 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100.

## Initial Thinking

- Try and understand: https://stackoverflow.com/a/4633515/3244654
    - Recursive permutation
- It can be considered as a combinatory problem where every separation item between the 9 digits is: `+`, or `-`, or `''`
- Will try brute-force approach

## Solution

- Implemented in TypeScript
- Verify: `npm start`
- It is using a recursive search where it build the combinations of
  operations and computes the result when finished
- Used a simple TDD approach using `assert` module and commenting cases
- Possible optimizations:
    - Better data structure for the operations array

## Sources

- https://adriann.github.io/programming_problems.html

    - Intermediate, number: 1
