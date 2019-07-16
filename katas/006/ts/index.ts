const assert = require("assert");

const swapNumsInArray = (arr: number[], idxA: number, idxB: number): void => {
  const tmp = arr[idxA];

  arr[idxA] = arr[idxB];
  arr[idxB] = tmp;
};

/*
https://en.wikipedia.org/wiki/Insertion_sort
i ← 1
while i < length(A)
    j ← i
    while j > 0 and A[j-1] > A[j]
        swap A[j] and A[j-1]
        j ← j - 1
    end while
    i ← i + 1
end while
*/
const insertSort = (nums: number[]): number[] => {
  const result = nums.slice(0);
  let i = 1;

  while (i < result.length) {
    let j = i;
    while (j > 0 && result[j - 1] > result[j]) {
      swapNumsInArray(result, j, j - 1);
      j -= 1;
    }
    i += 1;
  }

  return result;
};

const test = () => {
  assert.deepEqual(insertSort([0, 1, 2]), [0, 1, 2]);
  assert.deepEqual(insertSort([3, 2, 1, 0]), [0, 1, 2, 3]);
  assert.deepEqual(insertSort([1, 2, 1, 0]), [0, 1, 1, 2]);
};

const main = () => {
  test();
};

main();
