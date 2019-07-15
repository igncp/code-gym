const assert = require("assert");

type Operand = "" | "+" | "-";

interface Operation {
  num: number;
  operand: Operand;
}

const computeOperations = (operations: Array<Operation>): number => {
  const nums: Array<string> = operations
    .reduce(
      (acc, op: Operation) => {
        acc[acc.length - 1] = acc[acc.length - 1] + op.num;

        if (op.operand === "+") {
          acc.push("");
        } else if (op.operand === "-") {
          acc.push("-");
        }

        return acc;
      },
      [""]
    )
    .filter(i => !!i);

  return nums.reduce((acc, num) => {
    return acc + Number(num);
  }, 0);
};

const buildOperationsStr = (operations: Array<Operation>): string => {
  return operations.reduce((acc, op: Operation) => {
    return acc + op.num + op.operand;
  }, "");
};

const recursivelyCalculate = (
  possibilities: Array<string>,
  operations: Array<Operation>,
  remainderNumbers: Array<number>,
  result: number
) => {
  if (remainderNumbers.length > 0) {
    const nextNum = remainderNumbers[0];
    const restNums = remainderNumbers.slice(1, remainderNumbers.length);

    [""]
      .concat(remainderNumbers.length === 1 ? [] : ["+", "-"])
      .forEach((operand: Operand) => {
        recursivelyCalculate(
          possibilities,
          operations.concat({ num: nextNum, operand }),
          restNums,
          result
        );
      });
  } else {
    const operationNum = computeOperations(operations);

    if (process.env.DEBUG === "true") {
      console.log("OPERATIONS: ", operations, operationNum);
    }

    if (operationNum === result) {
      const operationStr = buildOperationsStr(operations);

      possibilities.push(operationStr);
    }
  }
};

const calculatePossibilities = (
  digits: string,
  result: number
): Array<string> => {
  const numbers = digits.split("").map(Number);
  const possibilities = [];

  recursivelyCalculate(possibilities, [], numbers, result);

  return possibilities;
};

const test = () => {
  assert.deepEqual(calculatePossibilities("12", 3), ["1+2"]);
  assert.deepEqual(calculatePossibilities("123456789", 100), [
    "123+45-67+8-9",
    "123+4-5+67-89",
    "123-45-67+89",
    "123-4-5-6-7+8-9",
    "12+3+4+5-6-7+89",
    "12+3-4+5+67+8+9",
    "12-3-4+5-6+7+89",
    "1+23-4+56+7+8+9",
    "1+23-4+5+6+78-9",
    "1+2+34-5+67-8+9",
    "1+2+3-4+5+6+78+9"
  ]);

  assert.deepEqual(computeOperations([{ num: 1, operand: "" }]), 1);
  assert.deepEqual(
    computeOperations([{ num: 1, operand: "-" }, { num: 2, operand: "" }]),
    -1
  );
  assert.deepEqual(
    computeOperations([{ num: 1, operand: "" }, { num: 2, operand: "" }]),
    12
  );

  assert.deepEqual(buildOperationsStr([{ num: 1, operand: "" }]), "1");
  assert.deepEqual(
    buildOperationsStr([{ num: 1, operand: "-" }, { num: 2, operand: "" }]),
    "1-2"
  );
  assert.deepEqual(
    buildOperationsStr([{ num: 1, operand: "" }, { num: 2, operand: "" }]),
    "12"
  );
};

const main = () => {
  test();
  const result = calculatePossibilities("123456789", 100);

  console.log("Result:");
  console.log(JSON.stringify(result, null, 4));
};

main();
