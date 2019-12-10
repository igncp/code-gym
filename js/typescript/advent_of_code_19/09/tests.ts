import {
  ExecutionPosition,
  IntcodeValue,
  Opcode,
  ParameterMode,
  RunProgramTillEndOpts,
  _test,
  convertStrToProgram,
  getHighestThrustersSignal,
  getVerbAndNounForResult,
  runProgramOperation,
  runProgramTillEnd
} from "./lib";

import { readFileSync } from "fs";

test.skip("part two", () => {
  const outputs2: number[] = [];
  const fileContent = readFileSync(__dirname + "/input.txt", "utf-8");

  const { program: newProgram } = runProgramTillEnd(
    { program: convertStrToProgram(fileContent), position: 0, relativeBase: 0 },
    {
      onInputRequest: () => 2,
      onOutputRequest: val => {
        outputs2.push(val);
      }
    }
  );

  // failing, seems it could be because BigInt
  expect(outputs2.length).not.toEqual(0);
});

describe("convertStrToProgram", () => {
  it("returns the expected program", () => {
    expect(convertStrToProgram("1,2,2,3")).toEqual([1, 2, 2, 3]);
  });
});

describe("runProgramOperation (previous exercise)", () => {
  it("returns the expected result for example 1", () => {
    const program = convertStrToProgram("1,9,10,3,2,3,11,0,99,30,40,50");
    const expectedResultProgram = convertStrToProgram(
      "1,9,10,70,2,3,11,0,99,30,40,50"
    );

    const result = runProgramOperation({
      position: 0,
      program,
      relativeBase: 0
    });

    expect(result).toEqual({
      position: 4,
      program: expectedResultProgram,
      relativeBase: 0
    });
  });

  it("returns the expected result for example 2", () => {
    const program = convertStrToProgram("1,9,10,70,2,3,11,0,99,30,40,50");
    const expectedResultProgram = convertStrToProgram(
      "3500,9,10,70,2,3,11,0,99,30,40,50"
    );

    const result = runProgramOperation({
      program,
      position: 4,
      relativeBase: 0
    });

    expect(result).toEqual({
      program: expectedResultProgram,
      position: 8,
      relativeBase: 0
    });
  });

  it("returns the expected result for example 3", () => {
    const program = convertStrToProgram("3500,9,10,70,2,3,11,0,99,30,40,50");

    const result = runProgramOperation({
      program,
      position: 8,
      relativeBase: 0
    });

    expect(result).toEqual({ program, position: 8, relativeBase: 0 });
  });

  it("throws on invalid opcode", () => {
    const program = convertStrToProgram("0,0,0,0");

    const fn = () =>
      runProgramOperation({ program, position: 0, relativeBase: 0 });

    expect(fn).toThrow("Invalid opcode: 0");
  });

  it("throws on missing functions", () => {
    expect(() =>
      runProgramOperation({
        program: convertStrToProgram("3,0,4,0,99"),
        position: 0,
        relativeBase: 0
      })
    ).toThrow("Missing onInputRequest");

    expect(() =>
      runProgramOperation({
        program: convertStrToProgram("3,0,4,0,99"),
        relativeBase: 0,
        position: 2
      })
    ).toThrow("Missing onOutputRequest");
  });
});

describe("runProgramOperation", () => {
  it("can handle different parameter modes", () => {
    expect(
      runProgramOperation({
        program: convertStrToProgram("1002,4,3,4,33"),
        relativeBase: 0,
        position: 0
      })
    ).toEqual({ program: [1002, 4, 3, 4, 99], position: 4, relativeBase: 0 });
  });
});

describe("runProgramTillEnd", () => {
  type Compare = (
    s: string,
    expected: [string, ExecutionPosition],
    options?: RunProgramTillEndOpts
  ) => void;

  const compare: Compare = (str, expected, options) => {
    const result = runProgramTillEnd(
      { program: convertStrToProgram(str), position: 0, relativeBase: 0 },
      options
    );

    expect(result).toEqual({
      relativeBase: 0,
      program: convertStrToProgram(expected[0]),
      position: expected[1]
    });
  };

  // eslint-disable-next-line jest/expect-expect
  it("returns the expected results for examples", () => {
    compare("1,0,0,0,99", ["2,0,0,0,99", 4]);
    compare("2,3,0,3,99", ["2,3,0,6,99", 4]);
    compare("2,4,4,5,99,0", ["2,4,4,5,99,9801", 4]);
    compare("1,1,1,4,99,5,6,0,99", ["30,1,1,4,2,5,6,0,99", 8]);
  });

  // eslint-disable-next-line jest/expect-expect
  it("can accept options", () => {
    compare("1,0,0,0,99", ["2,0,0,0,99", 4], {});
    compare("1,0,0,0,99", ["1,0,0,0,99", 0], { maxLoops: 0 });
  });

  // eslint-disable-next-line jest/expect-expect
  it("can handle output", () => {
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    const onOutputRequest = jest.fn(() => {});

    compare("3,0,4,0,99", ["10,0,4,0,99", 4], {
      onInputRequest: () => 10,
      onOutputRequest
    });

    expect(onOutputRequest.mock.calls).toEqual([[10]]);
  });

  it("produces the expected output from the examples", () => {
    let totalItems = 0;
    const longItem =
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";

    [
      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,9,8,9,10,9,4,9,99,-1,8"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 8,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,9,8,9,10,9,4,9,99,-1,8"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 7,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,9,7,9,10,9,4,9,99,-1,8"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 7,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,9,7,9,10,9,4,9,99,-1,8"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 8,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1108,-1,8,3,4,3,99"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 8,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1108,-1,8,3,4,3,99"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 7,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1107,-1,8,3,4,3,99"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 7,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1107,-1,8,3,4,3,99"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 8,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram(
              "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
            ),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 0,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1107,-1,8,3,4,3,99"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 2,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1105,-1,9,1101,0,0,12,4,12,99,1"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 0,
            onOutputRequest: val => {
              expect(val).toEqual(0);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram("3,3,1105,-1,9,1101,0,0,12,4,12,99,1"),
            relativeBase: 0,
            position: 0
          },
          {
            onInputRequest: () => 2,
            onOutputRequest: val => {
              expect(val).toEqual(1);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram(longItem),
            position: 0,
            relativeBase: 0
          },
          {
            onInputRequest: () => 7,
            onOutputRequest: val => {
              expect(val).toEqual(999);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram(longItem),
            position: 0,
            relativeBase: 0
          },
          {
            onInputRequest: () => 8,
            onOutputRequest: val => {
              expect(val).toEqual(1000);
            }
          }
        ),

      () =>
        runProgramTillEnd(
          {
            program: convertStrToProgram(longItem),
            position: 0,
            relativeBase: 0
          },
          {
            onInputRequest: () => 20,
            onOutputRequest: val => {
              expect(val).toEqual(1001);
            }
          }
        )
    ].forEach(fn => {
      fn();
      totalItems += 1;
    });

    expect.assertions(totalItems);
  });

  it("outputs the expected values for exercise 9 - 1 example", () => {
    const outputs: number[] = [];

    const program = convertStrToProgram(
      "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    );

    runProgramTillEnd(
      { program, position: 0, relativeBase: 0 },
      {
        onOutputRequest: val => {
          outputs.push(val);
        }
      }
    );

    expect(outputs.slice(0, program.length)).toEqual(program);
  });

  it("outputs the expected values for exercise 9 - 2 example", () => {
    const outputs: number[] = [];

    const program = convertStrToProgram("1102,34915192,34915192,7,4,7,99,0");

    runProgramTillEnd(
      { program, position: 0, relativeBase: 0 },
      {
        onOutputRequest: val => {
          outputs.push(val);
        }
      }
    );

    expect(outputs[0].toString().length).toEqual(16);
  });

  it("outputs the expected values for exercise 9 - 3 example", () => {
    const outputs: number[] = [];

    const middleNumber = 1125899906842624;
    const program = convertStrToProgram("104," + middleNumber + ",99");

    runProgramTillEnd(
      { program, position: 0, relativeBase: 0 },
      {
        onOutputRequest: val => {
          outputs.push(val);
        }
      }
    );

    expect(outputs).toEqual([middleNumber]);
  });

  it("outputs the expected values for exercise 9 - 3 extra example", () => {
    const outputs: number[] = [];

    const program = convertStrToProgram(
      "109,1,203,11,209,8,204,1,99,10,0,42,0"
    );

    runProgramTillEnd(
      { program, position: 0, relativeBase: 0 },
      {
        onInputRequest: () => 123,
        onOutputRequest: val => {
          outputs.push(val);
        }
      }
    );

    expect(outputs).toEqual([123]);
  });
});

describe("getVerbAndNounForResult", () => {
  it("returns the expected results for custom example", () => {
    type Compare = (
      s: string,
      result: IntcodeValue,
      expected: { verb: ExecutionPosition; noun: ExecutionPosition }
    ) => void;

    const compare: Compare = (str, result, expected) => {
      const actual = getVerbAndNounForResult(convertStrToProgram(str), result);
      expect(actual).toEqual(expected);
    };

    compare("1,7,4,0,99", 2, { noun: 0, verb: 0 });
    compare("1,7,4,0,99", 99, { noun: 4, verb: 3 });
  });

  it("throws when not found", () => {
    const fn = () =>
      getVerbAndNounForResult(convertStrToProgram("1,7,4,0,99"), 1000);
    expect(fn).toThrow("Not found");
  });
});

describe("getHighestThrustersSignal", () => {
  it("returns the expected value from examples", () => {
    expect(
      getHighestThrustersSignal(
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
      ).signal
    ).toEqual(43210);

    expect(
      getHighestThrustersSignal(
        "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
      ).signal
    ).toEqual(54321);

    expect(
      getHighestThrustersSignal(
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
      ).signal
    ).toEqual(65210);
  });

  it("returns the expected value from examples with feedback loop", () => {
    expect(
      getHighestThrustersSignal(
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
        true
      ).signal
    ).toEqual(139629729);

    expect(
      getHighestThrustersSignal(
        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10",
        true
      ).signal
    ).toEqual(18216);
  });
});

describe("_test", () => {
  const { getInstructionConfig } = _test!;

  describe("getInstructionConfig", () => {
    it("returns the expected values", () => {
      expect(getInstructionConfig(1002)).toEqual({
        paramsModes: {
          2: ParameterMode.Immediate,
          1: ParameterMode.Position
        },
        opcode: Opcode.Multiplication,
        positions: 4
      });

      expect(getInstructionConfig(3)).toEqual({
        paramsModes: {},
        opcode: Opcode.Input,
        positions: 2
      });

      expect(getInstructionConfig(5)).toEqual({
        paramsModes: {},
        opcode: Opcode.JumpIfTrue,
        positions: 3
      });
    });
  });
});
