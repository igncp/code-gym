import {requestNumber} from "./requestNumber";
import {RequesterResponseCb, BootstrapFn} from "./Interfaces";
import {getSumOfNumbers} from "./get-sum-of-numbers";

const showSumOfNumbers: RequesterResponseCb = (num: number): void => {
  const sum: number = getSumOfNumbers(num);
  console.log(`The result is: ${sum}`);
};

export const bootstrap: BootstrapFn = function(): void {
  requestNumber(showSumOfNumbers);
};
