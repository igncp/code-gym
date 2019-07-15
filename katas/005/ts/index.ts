const assert = require("assert");

const swapNumsInArray = (arr: number[], idxA: number, idxB: number): void => {
  const tmp = arr[idxA];

  arr[idxA] = arr[idxB];
  arr[idxB] = tmp;
};

const quicksortRec = (nums: number[], from: number, to: number): void => {
  if (from >= to) {
    return;
  }

  const pivot = nums[to];
  let splitIndex = from;

  for (let i = from; i <= to; i++) {
    if (nums[i] <= pivot) {
      swapNumsInArray(nums, splitIndex, i);

      splitIndex++;
    }
  }

  quicksortRec(nums, from, splitIndex - 2);
  quicksortRec(nums, splitIndex, to);
};

const quicksort = (nums: number[]): number[] => {
  const result = nums.slice(0, nums.length);

  quicksortRec(result, 0, result.length - 1);

  return result;
};

const test = () => {
  assert.deepEqual(quicksort([0, 1, 2]), [0, 1, 2]);
  assert.deepEqual(quicksort([3, 2, 1, 0]), [0, 1, 2, 3]);
  assert.deepEqual(quicksort([1, 2, 1, 0]), [0, 1, 1, 2]);
};

const main = () => {
  test();
};

main();
