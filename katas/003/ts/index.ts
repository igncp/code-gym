const assert = require("assert");

const getIfAnySumResultsNum = (
  nums: number[],
  result: number
): [number, number] | null => {
  const expectedNums = new Set<number>();

  for (let i = 0; i < nums.length; i++) {
    const num = nums[i];

    if (expectedNums.has(num)) {
      return [result - num, num];
    }

    const oppositeNum = result - num;

    expectedNums.add(oppositeNum);
  }

  return null;
};

const test = () => {
  assert.deepEqual(getIfAnySumResultsNum([], 10), null);
  assert.deepEqual(getIfAnySumResultsNum([1, 2], 3), [1, 2]);
  assert.deepEqual(getIfAnySumResultsNum([9, 1, 4, 5], 14), [9, 5]);
  assert.deepEqual(getIfAnySumResultsNum([9, 1, 4, 5], 15), null);
};

const main = () => {
  test();
};

main();
