import { isPasswordValid, getPasswordsNumInRange } from "./lib";

describe("isPasswordValid", () => {
  it("returns the expected values", () => {
    expect(isPasswordValid(0)).toEqual(false);
    expect(isPasswordValid(223450)).toEqual(false);
    expect(isPasswordValid(123789)).toEqual(false);

    expect(isPasswordValid(111111)).toEqual(true);
  });

  it("returns the expected values with onlyTwoPerGroup", () => {
    expect(isPasswordValid(0, true)).toEqual(false);
    expect(isPasswordValid(223450, true)).toEqual(false);
    expect(isPasswordValid(123789, true)).toEqual(false);
    expect(isPasswordValid(123444, true)).toEqual(false);
    expect(isPasswordValid(111111, true)).toEqual(false);

    expect(isPasswordValid(112233, true)).toEqual(true);
    expect(isPasswordValid(111122, true)).toEqual(true);
  });

  it("password is valid with onlyTwoPerGroup when at least one group is valid", () => {
    expect(isPasswordValid(133334, true)).toEqual(false);
    expect(isPasswordValid(133345, true)).toEqual(false);
    expect(isPasswordValid(133344, true)).toEqual(true);
  });
});

describe("getPasswordsNumInRange", () => {
  it("returns the expected number", () => {
    expect(getPasswordsNumInRange("111111-111121")).toEqual(9);
    expect(getPasswordsNumInRange("111111-111121", true)).toEqual(0);
  });
});
