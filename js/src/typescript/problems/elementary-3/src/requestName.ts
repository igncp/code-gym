/// <reference path="../typings/globals/node/index.d.ts" />

import {Requester, RequesterResponseCb} from "./Interfaces";

export const requestName: Requester = function(cb: RequesterResponseCb): void {
  console.log("Please, enter your name:");

  process.stdin.resume();
  process.stdin.setEncoding("utf8");

  process.stdin.on("data", (text: string): void => {
    process.stdin.pause();
    cb(text.trim());
  });
};
