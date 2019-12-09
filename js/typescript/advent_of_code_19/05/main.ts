import { readFileSync } from "fs";
import assert from "assert";

import { convertStrToProgram, runProgramTillEnd } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const originalProgram = convertStrToProgram(fileContent);
  const outputs: number[] = [];

  runProgramTillEnd([originalProgram, 0], {
    onInputRequest: () => 1,
    onOutputRequest: (val: number) => {
      outputs.push(val);
    }
  });

  const firstResultOutput = outputs[outputs.length - 1];

  assert(firstResultOutput === 16574641);

  console.log("Result:");
  console.log("1. First value: " + firstResultOutput);
};

main();
