import { readFileSync } from "fs";
import assert from "assert";

import {
  convertStrToProgram,
  runProgramTillEnd,
  getVerbAndNounForResult
} from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const originalProgram = convertStrToProgram(fileContent);

  const firstProgram = originalProgram.slice(0);

  firstProgram[1] = 12;
  firstProgram[2] = 2;

  const [firstProgramResult] = runProgramTillEnd([firstProgram, 0]);

  assert(firstProgramResult[0] === 3716250);

  console.log("Result:");
  console.log("1. value at position 0: " + firstProgramResult[0]);

  const requestedResult = 19690720;
  const { verb, noun } = getVerbAndNounForResult(firstProgram, requestedResult);
  const secondResult = verb + noun * 100;

  console.log("2. 100 * noun + verb: " + secondResult);

  assert(secondResult === 6472);
};

main();
