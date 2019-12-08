import { readFileSync } from "fs";
import assert from "assert";

import {
  getFewestCombinedStepsOfCrossing,
  getManhattanDistanceOfClosestCrossing,
  parseLineIntoWirePath
} from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const wirePaths = fileContent
    .split("\n")
    .filter(l => !!l)
    .map(parseLineIntoWirePath);

  const firstResult = getManhattanDistanceOfClosestCrossing(
    wirePaths[0],
    wirePaths[1]
  );
  const secondResult = getFewestCombinedStepsOfCrossing(
    wirePaths[0],
    wirePaths[1]
  );

  assert(firstResult === 1017);
  assert(secondResult === 11432);

  console.log("Result:");
  console.log("1. manhattan distance: " + firstResult);
  console.log("2. fewest steps: " + secondResult);
};

main();
