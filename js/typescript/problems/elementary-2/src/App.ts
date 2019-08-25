import { requestName } from "./requestName";
import { RequesterResponseCb, IApp } from "./Interfaces";

const greet: RequesterResponseCb = (name: string): void => {
  console.log(`Hi, ${name}`);
};

export class App implements IApp {
  bootstrap(): void {
    requestName(greet);
  }
}
