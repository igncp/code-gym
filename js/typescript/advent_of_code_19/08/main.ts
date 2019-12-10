import { readFileSync } from "fs";
import assert from "assert";

import { getImageVerificationNumber, getLayersResultImage } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const dimensions = { width: 25, height: 6 };
  const firstResult = getImageVerificationNumber(dimensions, fileContent);
  const secondResult = getLayersResultImage(dimensions, fileContent);
  const cleanedImg = secondResult
    .replace(/0/g, " ")
    .trim()
    .split("\n")
    .map(l => l.trim())
    .join("\n");

  const expectedImg = `
1      11 1111  11  1  1
1       1 1    1  1 1  1
1       1 111  1    1111
1       1 1    1    1  1
1    1  1 1    1  1 1  1
1111  11  1111  11  1  1`.trim();

  assert(expectedImg === cleanedImg);
  assert(firstResult === 1792);

  console.log("Result:");
  console.log("1. First value: " + firstResult);
  console.log("2. Resulting image:");
  console.log(cleanedImg);
};

main();
