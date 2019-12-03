import {
  getFuelForMass,
  getFuelForModules,
  convertFileContentIntoModules
} from "./lib";

describe("getFuelForMass", () => {
  it("returns the expected values without including fuel", () => {
    expect(getFuelForMass(12)).toEqual(2);
    expect(getFuelForMass(14)).toEqual(2);
    expect(getFuelForMass(1969)).toEqual(654);
    expect(getFuelForMass(100756)).toEqual(33583);
  });

  it("returns the expected values including fuel", () => {
    expect(getFuelForMass(14, true)).toEqual(2);
    expect(getFuelForMass(1969, true)).toEqual(966);
    expect(getFuelForMass(100756, true)).toEqual(50346);
  });
});

describe("getFuelFoModules", () => {
  it("returns the expected values", () => {
    expect(getFuelForModules([12, 12])).toEqual(4);
    expect(getFuelForModules([1969, 1969])).toEqual(654 * 2);
    expect(getFuelForModules([1969, 1969], true)).toEqual(966 * 2);
  });
});

describe("convertFileContentIntoModules", () => {
  it("gets a list of modules", () => {
    const example = `1
2`;
    expect(convertFileContentIntoModules(example)).toEqual([1, 2]);
  });
});
