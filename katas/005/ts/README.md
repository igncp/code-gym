## Problem

- Implement QuickSort algorithm in TS
- Indicate what to change to change result order

## Comments

- Added the `npm run inspect` command
- To reverse the order, just the pivot comparison needs to be changed
    - From `if (nums[i] <= pivot) {`
    - To `if (nums[i] >= pivot) {`
