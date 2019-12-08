type IsPasswordValid = (p: number, onlyTwoPerGroup?: boolean) => boolean;

const isPasswordValid: IsPasswordValid = (p, onlyTwoPerGroup) => {
  if (p < 99999 || p > 999999) {
    return false;
  }

  const nums = p
    .toString()
    .split("")
    .map(c => Number(c));
  const equalGroups: { [k: string]: number } = {};

  for (let chIdx = 1; chIdx < 6; chIdx += 1) {
    const num = nums[chIdx];
    const prev = nums[chIdx - 1];

    if (num < prev) {
      return false;
    }

    if (num === prev) {
      equalGroups[num] = (equalGroups[num] || 1) + 1;
    }
  }

  const keys: string[] = Object.keys(equalGroups);

  if (!keys.length) {
    return false;
  }

  return !onlyTwoPerGroup || keys.some(k => equalGroups[k] === 2);
};

type GetPasswordsNumInRange = (
  range: string,
  onlyTwoPerGroup?: boolean
) => number;

const getPasswordsNumInRange: GetPasswordsNumInRange = (
  range,
  onlyTwoPerGroup
) => {
  const [min, max] = range.split("-").map(n => Number(n));
  let total = 0;

  for (let num = min; num <= max; num += 1) {
    if (isPasswordValid(num, onlyTwoPerGroup)) {
      total += 1;
    }
  }

  return total;
};

export { isPasswordValid, getPasswordsNumInRange };
