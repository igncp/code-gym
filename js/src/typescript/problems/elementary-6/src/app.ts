/// <reference path="../typings/modules/bluebird/index.d.ts" />
/// <reference path="../typings/modules/bluebird/index.d.ts" />

import * as Promise from "bluebird";

import {requestNumber} from "./request-number";
import {BootstrapFn, Strategy} from "./interfaces";
import {strategies, strategyToFnMap, whitelistedMultiples} from "./constants";

type StrategyWithNum = {
  strategy: Strategy,
  num: number,
}

function askForStrategy(num: number): Promise<StrategyWithNum> {
  return new Promise(function(resolve: any): void {
    process.stdin.resume();
    process.stdin.setEncoding("utf8");
    console.log(`Please, choose between '${strategies["SUM"]}' ` +
      `(just using multiples of ${whitelistedMultiples.join(", ")}) ` +
      `or '${strategies["MULTIPLICATION"]}' (using all numbers)`);

    function listener(text: string): void {
      const response: string = text.trim();
      if (response === strategies["SUM"] || response === strategies["MULTIPLICATION"]) {
        process.stdin.pause();
        process.stdin.removeListener("data", listener);
        resolve({
          strategy: response,
          num,
        });
      } else {
        console.log(`Please, enter '${strategies["SUM"]}' or '${strategies["MULTIPLICATION"]}'`);
      }
    }

    process.stdin.on("data", listener);
  }) as Promise<StrategyWithNum>;
}

function performStrategy(obj: StrategyWithNum): void {
  const result: number = strategyToFnMap[obj.strategy](obj.num);

  console.log(`The result is ${result} using ${obj.strategy} in ${obj.num}`);
}

export const bootstrap: BootstrapFn = function(): void {
  requestNumber()
    .then(askForStrategy)
    .then(performStrategy);
};
