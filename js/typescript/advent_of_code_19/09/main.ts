import { readFileSync } from "fs";
import assert from "assert";

import { runProgramTillEnd, convertStrToProgram } from "./lib";

const main = () => {
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const program = convertStrToProgram(fileContent);

  const outputs1: number[] = [];
  const outputs2: number[] = [];

  runProgramTillEnd(
    { program, position: 0, relativeBase: 0 },
    {
      onInputRequest: () => 1,
      onOutputRequest: val => {
        outputs1.push(val);
      }
    }
  );

  const { program: newProgram } = runProgramTillEnd(
    { program, position: 0, relativeBase: 0 },
    {
      onInputRequest: () => 1,
      onOutputRequest: val => {
        outputs2.push(val);
      }
    }
  );

  assert(outputs1[0] === 2316632620);
  assert(outputs1.length === 1);

  console.log("outputs2", outputs2.length);

  console.log("Result:");
  console.log("1. First value: " + outputs1[0]);
  console.log("2. Second value: " + outputs2[0]);
};

main();
