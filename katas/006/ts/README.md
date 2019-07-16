## Problem

- Implement Insert Sort algorithm in TS
- Indicate what to change to change result order

## Comments

- The required change for reversing the ordering is:
    - From `while (j > 0 && result[j - 1] > result[j]) `
    - To `while (j > 0 && result[j - 1] < result[j]) `
