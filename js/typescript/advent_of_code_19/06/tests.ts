import {
  getDirectAndInderctOrbitsNum,
  getMinimumOrbitTransfers,
  _test
} from "./lib";

const orbitsStr = `
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
`;

const orbitsStr2 = `
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
`;

describe("getDirectAndInderctOrbitsNum", () => {
  it("returns the example value", () => {
    expect(getDirectAndInderctOrbitsNum(orbitsStr)).toEqual(42);
  });
});

describe("getMinimumOrbitTransfers", () => {
  it("returns the expected value of the example", () => {
    expect(getMinimumOrbitTransfers(orbitsStr2, ["YOU", "SAN"])).toEqual(4);
  });
});

describe("_test", () => {
  const { getAllOrbitedForObj, transformStrIntoOrbitsMap } = _test!;

  const orbitsMap = transformStrIntoOrbitsMap(orbitsStr);

  describe("getAllOrbitedForObj", () => {
    it("returns the expected values", () => {
      expect(getAllOrbitedForObj("C", orbitsMap)).toEqual(["B", "COM"]);
    });
  });

  describe("transformStrIntoOrbitsMap", () => {
    it("returns error when duplicate orbit", () => {
      expect(() =>
        transformStrIntoOrbitsMap(`
FOO)BAR
BAZ)BAR
`)
      ).toThrow("Unexpected duplicate orbit: BAR");
    });
  });
});
