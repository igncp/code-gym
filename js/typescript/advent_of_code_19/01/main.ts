import { readFileSync } from "fs";

import { convertFileContentIntoModules, getFuelForModules } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const modules = convertFileContentIntoModules(fileContent);

  console.log("Result:");
  console.log("1. total fuel: " + getFuelForModules(modules));
  console.log("2. total fuel: " + getFuelForModules(modules, true));
};

main();
