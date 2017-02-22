/// <reference path="../typings/globals/node/index.d.ts" />
/// <reference path="../typings/modules/bluebird/index.d.ts" />

import {Requester} from "./interfaces";
import * as Promise from "bluebird";

export const requestNumber: Requester = function(): Promise<number> {
  console.log("Please, enter a number:");

  process.stdin.resume();
  process.stdin.setEncoding("utf8");

  return new Promise(function(resolve: any): void {
    function listener(text: string): void {
      const num: number = parseInt(text.trim());

      if (isNaN(num)) {
        console.log("Please, enter a valid number");
      } else {
        process.stdin.pause();
        process.stdin.removeListener("data", listener);
        resolve(num);
      }
    }

    process.stdin.on("data", listener);
  }) as Promise<number>;
};
