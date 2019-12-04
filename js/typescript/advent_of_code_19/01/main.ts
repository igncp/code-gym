import assert from "assert";

import { readFileSync } from "fs";

import { convertFileContentIntoModules, getFuelForModules } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const modules = convertFileContentIntoModules(fileContent);
  const firstResult = getFuelForModules(modules);
  const secondResult = getFuelForModules(modules, true);

  assert(firstResult === 3373568);
  assert(secondResult === 5057481);

  console.log("Result:");
  console.log("1. total fuel: " + firstResult);
  console.log("2. total fuel: " + secondResult);
};

main();
