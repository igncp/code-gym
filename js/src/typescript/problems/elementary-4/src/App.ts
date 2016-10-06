import {requestNumber} from "./requestNumber";
import {RequesterResponseCb, BootstrapFn} from "./Interfaces";

// recursion would not accept long numbers
function getSumOfNumbers(num: number): number {
  if (num > 0) {
    let sum: number = 0;
    for (let n: number = num; n > 0; n--) {
      sum += n;
    }
    return sum;
  } else return num;
}

const showSumOfNumbers: RequesterResponseCb = (num: number): void => {
  const sum: number = getSumOfNumbers(num);
  console.log(`The result is: ${sum}`);
};

export const bootstrap: BootstrapFn = function(): void {
  requestNumber(showSumOfNumbers);
};
