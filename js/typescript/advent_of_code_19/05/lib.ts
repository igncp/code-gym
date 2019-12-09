enum Opcode {
  Addition = 1,
  Multiplication = 2,
  Input = 3,
  Output = 4,
  JumpIfTrue = 5,
  JumpIfFalse = 6,
  LessThan = 7,
  Equals = 8,
  End = 99
}

enum ParameterMode {
  Position = 0,
  Immediate = 1
}

type IntcodeValue = number;

type IntcodeProgram = IntcodeValue[];

type ExecutionPosition = number;

type IntcodeProgramState = [IntcodeProgram, ExecutionPosition];

type ParamsMods = { [pos: string]: ParameterMode };

type GetInstructionConfig = (
  c: number
) => {
  paramsModes: ParamsMods;
  opcode: Opcode;
  positions: number;
};

const getInstructionConfig: GetInstructionConfig = num => {
  const numStr = num.toString();
  let opcodeNum = num;
  let positions = 4;
  const paramsModes: ParamsMods = {};

  if (numStr.length > 2) {
    opcodeNum = Number(numStr.substr(numStr.length - 2, numStr.length));
    const paramsModeStart = numStr.length - 3;

    for (let numPos = paramsModeStart; numPos >= 0; numPos -= 1) {
      const paramPos = paramsModeStart - numPos + 1;

      paramsModes[paramPos] =
        numStr[numPos] === "1"
          ? ParameterMode.Immediate
          : ParameterMode.Position;
    }
  }

  if (opcodeNum === Opcode.Input || opcodeNum === Opcode.Output) {
    positions = 2;
  } else if (opcodeNum === Opcode.End) {
    positions = 0;
  } else if (!Object.values(Opcode).includes(opcodeNum)) {
    throw new Error("Invalid opcode: " + opcodeNum);
  }

  return {
    opcode: opcodeNum,
    positions,
    paramsModes
  };
};

interface RunProgramOpts {
  onInputRequest?(): number;
  onOutputRequest?(p: number): void;
}

type RunProgramOperation = (
  programState: IntcodeProgramState,
  opts?: RunProgramOpts
) => IntcodeProgramState;

const runProgramOperation: RunProgramOperation = (
  [program, position],
  opts = {}
) => {
  const newProgram = program.slice(0);

  const { positions, opcode, paramsModes } = getInstructionConfig(
    newProgram[position]
  );
  const newPosition = position + positions;

  const getInput = (posNum: number): number => {
    const inputPosition = newProgram[position + posNum];

    return paramsModes[posNum] === ParameterMode.Immediate
      ? inputPosition
      : newProgram[inputPosition];
  };

  const setOutput = (posNum: number, val: number) => {
    const outputPosition = newProgram[position + posNum];

    newProgram[outputPosition] = val;
  };

  if (opcode === Opcode.Addition) {
    const result = getInput(1) + getInput(2);
    setOutput(3, result);
  } else if (opcode === Opcode.Multiplication) {
    const result = getInput(1) * getInput(2);
    setOutput(3, result);
  } else if (opcode === Opcode.Input) {
    if (!opts.onInputRequest) {
      throw new Error("Missing onInputRequest");
    }

    const result = opts.onInputRequest();

    setOutput(1, result);
  } else if (opcode === Opcode.Output) {
    if (!opts.onOutputRequest) {
      throw new Error("Missing onOutputRequest");
    }

    const result = getInput(1);

    opts.onOutputRequest(result);
  }

  return [newProgram, newPosition];
};

type RunProgramTillEndOpts = {
  maxLoops?: number;
} & RunProgramOpts;

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
    [newProgram, newPosition] = runProgramOperation([newProgram, newPosition], {
      onInputRequest: opts.onInputRequest,
      onOutputRequest: opts.onOutputRequest
    });
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

type Test = null | {
  getInstructionConfig: GetInstructionConfig;
};

let _test: Test = null;

// istanbul ignore else
if (typeof __TEST__ !== "undefined") {
  _test = {
    getInstructionConfig
  };
}

export {
  ExecutionPosition,
  IntcodeProgram,
  IntcodeProgramState,
  IntcodeValue,
  Opcode,
  ParameterMode,
  RunProgramTillEndOpts,
  _test,
  convertStrToProgram,
  getVerbAndNounForResult,
  runProgramOperation,
  runProgramTillEnd
};
