## Problem

Write a program that outputs all possibilities to put + or - or nothing between
the numbers 1,2,…,9 (in this order) such that the result is 100. For example 1
+ 2 + 3 - 4 + 5 + 6 + 78 + 9 = 100.

## Initial Thinking

- It can be considered as a combinatory problem where every separation item
  between the 9 digits is: `'+'`, or `'-'`, or `''`

## Solution

- Verify: `npm start`
- Possible optimizations:
    - Better data structure for the operations array
    - Stop branch when result too high with remaining numbers

## Sources

- https://adriann.github.io/programming_problems.html
    - Intermediate, number: 1
