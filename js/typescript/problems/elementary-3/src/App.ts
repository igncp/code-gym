import {requestName} from "./requestName";
import {whitelistedNames} from "./constants";
import {RequesterResponseCb, BootstrapFn} from "./Interfaces";

const greet: RequesterResponseCb = (name: string): void => {
  if (whitelistedNames.indexOf(name) > -1) {
    console.log(`Hi, ${name}!`);
  } else {
    console.log("Hi...");
  }
};

export const bootstrap: BootstrapFn = function(): void {
  requestName(greet);
};

