import { readFileSync } from "fs";
import assert from "assert";

import { getHighestThrustersSignal } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const { signal: firstResult } = getHighestThrustersSignal(fileContent);
  const { signal: secondResult } = getHighestThrustersSignal(fileContent, true);

  assert(firstResult === 46014);
  assert(firstResult === 19581200);

  console.log("Result:");
  console.log("1. First value: " + firstResult);
  console.log("2. Second value: " + secondResult);
};

main();
