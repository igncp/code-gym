## Problem

For an array of numbers, with a number `n`, calculate if two numbers from the
array sum `n`. E.g. `[9, 1, 4, 5]` and `n` of `14`, return `[9, 5]`, otherwise
`null`.

## Initial Thinking

- The loop can break early
- Will try to implement in one pass using a set and calculating the difference in advance
- Because Set lookup is `O(1)`, this approach is `O(N)` on worst case

## Solution

- Run: `npm i && npm start`

## Other

- Included `tsconfig.json` which more checks
