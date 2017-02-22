import {whitelistedMultiples} from "./constants";

function isWhitelistedMultiple(num: number): boolean {
  for (let i: number = 0; i < whitelistedMultiples.length; i++) {
    if (num % whitelistedMultiples[i] === 0) {
      return true;
    }
  }

  return false;
}

// recursion would not accept long numbers
export function getSumOfNumbers(num: number): number {
  if (num > 0) {
    let sum: number = 0;
    for (let n: number = num; n > 0; n--) {
      if (isWhitelistedMultiple(n)) {
        sum += n;
      }
    }
    return sum;
  } else return num;
}

