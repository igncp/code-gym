import { readFileSync } from "fs";
import assert from "assert";

import { getPasswordsNumInRange } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const firstResult = getPasswordsNumInRange(fileContent.trim());
  const secondResult = getPasswordsNumInRange(fileContent.trim(), true);

  assert(firstResult === 1150);
  assert(secondResult === 748);

  console.log("Result:");
  console.log("1. number of passwords: " + firstResult);
  console.log("2. number of passwords: " + secondResult);
};

main();
