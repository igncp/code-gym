// recursion would not accept long numbers
export function getMultiplicationOfNumbers(num: number): number {
  if (num > 0) {
    let mult: number = num;
    for (let n: number = num - 1; n > 0; n--) {
      mult *= n;
    }
    return mult;
  } else return num;
}

