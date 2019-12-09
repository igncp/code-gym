import { readFileSync } from "fs";
import assert from "assert";

import { convertStrToProgram, runProgramTillEnd } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const originalProgram = convertStrToProgram(fileContent);

  const outputsFirst: number[] = [];
  const outputsSecond: number[] = [];

  runProgramTillEnd([originalProgram, 0], {
    onInputRequest: () => 1,
    onOutputRequest: (val: number) => {
      outputsFirst.push(val);
    }
  });

  runProgramTillEnd([originalProgram, 0], {
    onInputRequest: () => 5,
    onOutputRequest: (val: number) => {
      outputsSecond.push(val);
    }
  });

  const firstResultOutput = outputsFirst[outputsFirst.length - 1];
  const secondResultOutput = outputsSecond[0];

  assert(firstResultOutput === 16574641);
  assert(firstResultOutput === 15163975);

  console.log("Result:");
  console.log("1. First value: " + firstResultOutput);
  console.log("1. Second value: " + secondResultOutput);
};

main();
