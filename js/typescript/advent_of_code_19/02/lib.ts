enum Opcode {
  Addition = 1,
  Multiplication = 2,
  End = 99
}

type IntcodeValue = number;

type IntcodeProgram = Array<IntcodeValue>;

type ExecutionPosition = number;

type IntcodeProgramState = [IntcodeProgram, ExecutionPosition];

type RunProgramOperation = (
  programState: IntcodeProgramState
) => IntcodeProgramState;

const runProgramOperation: RunProgramOperation = ([program, position]) => {
  const newProgram = program.slice(0);
  let newPosition = position;

  const inputPositionA = newProgram[position + 1];
  const inputPositionB = newProgram[position + 2];
  const outputPosition = newProgram[position + 3];

  if (newProgram[position] === Opcode.Addition) {
    newProgram[outputPosition] =
      newProgram[inputPositionA] + newProgram[inputPositionB];
    newPosition += 4;
  } else if (newProgram[position] === Opcode.Multiplication) {
    newProgram[outputPosition] =
      newProgram[inputPositionA] * newProgram[inputPositionB];
    newPosition += 4;
  } else if (newProgram[position] !== Opcode.End) {
    throw new Error("Invalid opcode: " + newProgram[position]);
  }

  return [newProgram, newPosition];
};

interface RunProgramTillEndOpts {
  maxLoops?: number;
}

type RunProgramTillEnd = (
  s: IntcodeProgramState,
  opts?: RunProgramTillEndOpts
) => IntcodeProgramState;

const runProgramTillEnd: RunProgramTillEnd = (
  [program, position],
  opts = {}
) => {
  const maxLoops = typeof opts.maxLoops === "undefined" ? 1000 : opts.maxLoops;

  let iterations = 0;
  let newProgram = program.slice(0);
  let newPosition = position;

  while (newProgram[newPosition] !== Opcode.End && iterations < maxLoops) {
    [newProgram, newPosition] = runProgramOperation([newProgram, newPosition]);
    iterations += 1;
  }

  return [newProgram, newPosition];
};

type GetVerbAndNounForResult = (
  p: IntcodeProgram,
  result: IntcodeValue
) => { verb: ExecutionPosition; noun: ExecutionPosition };

const getVerbAndNounForResult: GetVerbAndNounForResult = (program, result) => {
  for (let verb = 0; verb <= 99; verb += 1) {
    for (let noun = 0; noun <= 99; noun += 1) {
      const programCopy = program.slice(0);

      programCopy[1] = noun;
      programCopy[2] = verb;

      const [programResult] = runProgramTillEnd([programCopy, 0]);

      if (programResult[0] === result) {
        return {
          verb,
          noun
        };
      }
    }
  }

  throw new Error("Not found");
};

type ConvertStrToProgram = (str: string) => IntcodeProgram;

const convertStrToProgram: ConvertStrToProgram = str => {
  return str
    .split(",")
    .filter(n => !!n)
    .map(n => Number(n));
};

export {
  ExecutionPosition,
  IntcodeProgram,
  IntcodeProgramState,
  IntcodeValue,
  RunProgramTillEndOpts,
  convertStrToProgram,
  getVerbAndNounForResult,
  runProgramOperation,
  runProgramTillEnd
};
