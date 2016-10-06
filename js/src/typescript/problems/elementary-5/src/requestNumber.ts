/// <reference path="../typings/globals/node/index.d.ts" />

import {Requester, RequesterResponseCb} from "./Interfaces";

export const requestNumber: Requester = function(cb: RequesterResponseCb): void {
  console.log("Please, enter a number:");

  process.stdin.resume();
  process.stdin.setEncoding("utf8");

  process.stdin.on("data", (text: string): void => {
    const num: number = parseInt(text.trim());

    if (isNaN(num)) {
      console.log("Please, enter a valid number");
    } else {
      process.stdin.pause();
      cb(num);
    }

  });
};
