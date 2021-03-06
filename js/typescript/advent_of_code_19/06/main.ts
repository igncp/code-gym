import { readFileSync } from "fs";
import assert from "assert";

import { getDirectAndInderctOrbitsNum, getMinimumOrbitTransfers } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const firstResult = getDirectAndInderctOrbitsNum(fileContent);
  const secondResult = getMinimumOrbitTransfers(fileContent, ["YOU", "SAN"]);

  console.log("Result:");
  console.log("1. First value: " + firstResult);
  console.log("2. Second value: " + secondResult);

  assert(firstResult === 171213);
  assert(secondResult === 292);
};

main();
