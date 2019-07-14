const assert = require("assert");

const BASIC_SYMBOLS: { [ch: string]: number } = {
  C: 100,
  I: 1,
  M: 1000,
  X: 10
};

const AUXILIARY_SYMBOLS: { [ch: string]: number } = {
  D: 500,
  L: 50,
  V: 5
};

const Errors = {
  InvalidAddition: "Invalid numeral: additions don't decrease.",
  InvalidAuxiliarySub: "Invalid numeral: can't subtract auxiliary symbol.",
  InvalidCh: "Invalid character in input. Valid characters are I,V,X,L,C,D,M.",
  InvalidConsecutiveSub: "Invalid numeral: two consecutive subtractions."
};

const BASIC_SYMBOLS_CH = Object.keys(BASIC_SYMBOLS);
const AUXILIARY_SYMBOLS_CH = Object.keys(AUXILIARY_SYMBOLS);
const SUPPORTED_CH = BASIC_SYMBOLS_CH.concat(AUXILIARY_SYMBOLS_CH);

const convertLineToArabic = (line: string): string => {
  let result = 0;
  let previousVal = 0;
  let wasSubtracted = false;
  const additions = [Infinity];

  for (let i = 0; i < line.length; i++) {
    const ch: string = line[i];

    if (SUPPORTED_CH.indexOf(ch) === -1) {
      return Errors.InvalidCh;
    }

    const val: number = BASIC_SYMBOLS[ch] || AUXILIARY_SYMBOLS[ch];
    let addition = val;

    if (previousVal && previousVal < val) {
      if (wasSubtracted) {
        return Errors.InvalidConsecutiveSub;
      }

      const previousChar = line[i - 1] || "";

      if (AUXILIARY_SYMBOLS_CH.indexOf(previousChar) !== -1) {
        return Errors.InvalidAuxiliarySub;
      }

      addition -= previousVal * 2;
      additions.pop();
      wasSubtracted = true;
    } else {
      wasSubtracted = false;
    }

    if (addition > additions[additions.length - 1]) {
      return Errors.InvalidAddition;
    }

    if (!wasSubtracted) {
      additions.push(addition);
    }

    result += addition;
    previousVal = val;
  }

  return String(result || "");
};

const converToArabicNumbers = (romanNumbers: string): string => {
  return romanNumbers
    .split("\n")
    .map(convertLineToArabic)
    .filter(l => !!l)
    .join("\n");
};

const test = () => {
  assert.deepEqual(converToArabicNumbers(""), "");
  assert.deepEqual(converToArabicNumbers("X"), "10");
  assert.deepEqual(converToArabicNumbers("XII"), "12");
  assert.deepEqual(converToArabicNumbers("IV"), "4");
  assert.deepEqual(converToArabicNumbers("XL"), "40");
  assert.deepEqual(converToArabicNumbers("CLXXXIII"), "183");
  assert.deepEqual(converToArabicNumbers("DLV"), "555");
  assert.deepEqual(converToArabicNumbers("MDLXXXII"), "1582");

  assert.deepEqual(converToArabicNumbers("a"), Errors.InvalidCh);
  assert.deepEqual(converToArabicNumbers("LM"), Errors.InvalidAuxiliarySub);
  assert.deepEqual(converToArabicNumbers("LIVX"), Errors.InvalidConsecutiveSub);
  assert.deepEqual(converToArabicNumbers("LIIX"), Errors.InvalidAddition);

  assert.deepEqual(converToArabicNumbers(`V\nX`), `5\n10`);
};

const main = () => {
  test();
};

main();
