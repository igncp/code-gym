import {
  ExecutionPosition,
  IntcodeValue,
  RunProgramTillEndOpts,
  convertStrToProgram,
  getVerbAndNounForResult,
  runProgramOperation,
  runProgramTillEnd
} from "./lib";

describe("convertStrToProgram", () => {
  it("returns the expected program", () => {
    expect(convertStrToProgram("1,2,2,3")).toEqual([1, 2, 2, 3]);
  });
});

describe("runProgramOperation", () => {
  it("returns the expected result for example 1", () => {
    const program = convertStrToProgram("1,9,10,3,2,3,11,0,99,30,40,50");
    const expectedResultProgram = convertStrToProgram(
      "1,9,10,70,2,3,11,0,99,30,40,50"
    );

    const result = runProgramOperation([program, 0]);

    expect(result).toEqual([expectedResultProgram, 4]);
  });

  it("returns the expected result for example 2", () => {
    const program = convertStrToProgram("1,9,10,70,2,3,11,0,99,30,40,50");
    const expectedResultProgram = convertStrToProgram(
      "3500,9,10,70,2,3,11,0,99,30,40,50"
    );

    const result = runProgramOperation([program, 4]);

    expect(result).toEqual([expectedResultProgram, 8]);
  });

  it("returns the expected result for example 3", () => {
    const program = convertStrToProgram("3500,9,10,70,2,3,11,0,99,30,40,50");

    const result = runProgramOperation([program, 8]);

    expect(result).toEqual([program, 8]);
  });

  it("throws on invalid opcode", () => {
    const program = convertStrToProgram("0,0,0,0");

    const fn = () => runProgramOperation([program, 0]);

    expect(fn).toThrow("Invalid opcode: 0");
  });
});

describe("runProgramTillEnd", () => {
  type Compare = (
    s: string,
    expected: [string, ExecutionPosition],
    options?: RunProgramTillEndOpts
  ) => void;
  const compare: Compare = (str, expected, options) => {
    const result = runProgramTillEnd([convertStrToProgram(str), 0], options);
    expect(result).toEqual([convertStrToProgram(expected[0]), expected[1]]);
  };

  it("returns the expected results for examples", () => {
    compare("1,0,0,0,99", ["2,0,0,0,99", 4]);
    compare("2,3,0,3,99", ["2,3,0,6,99", 4]);
    compare("2,4,4,5,99,0", ["2,4,4,5,99,9801", 4]);
    compare("1,1,1,4,99,5,6,0,99", ["30,1,1,4,2,5,6,0,99", 8]);
  });

  it("can accept options", () => {
    compare("1,0,0,0,99", ["2,0,0,0,99", 4], {});
    compare("1,0,0,0,99", ["1,0,0,0,99", 0], { maxLoops: 0 });
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
