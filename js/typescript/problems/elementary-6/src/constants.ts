import {Strategy} from "./interfaces";
import {getSumOfNumbers} from "./get-sum-of-numbers";
import {getMultiplicationOfNumbers} from "./get-multiplication-of-numbers";

export const strategies: { [key: string]: Strategy } = {
  SUM: "sum",
  MULTIPLICATION: "multiplication",
};

export const strategyToFnMap: { [key: string]: Function } = {
  [strategies["SUM"]]: getSumOfNumbers,
  [strategies["MULTIPLICATION"]]: getMultiplicationOfNumbers,
};

export const whitelistedMultiples: number[] = [
  3,
  5,
];
