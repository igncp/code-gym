import {
  WireDirection,
  _test,
  convertWirePathsIntoMapStr,
  getFewestCombinedStepsOfCrossing,
  getManhattanDistanceOfClosestCrossing,
  parseLineIntoWirePath
} from "./lib";

const { serializePosition, deserializePosition } = _test!;

describe("parseLineIntoWirePath", () => {
  it("returns the expected result", () => {
    expect(parseLineIntoWirePath("R8,U5")).toEqual([
      [WireDirection.Right, 8],
      [WireDirection.Up, 5]
    ]);
  });

  it("throws when unexpected letter", () => {
    expect(() => parseLineIntoWirePath("R8,F5")).toThrow("Invalid Char");
  });
});

describe("convertWirePathIntoMapStr", () => {
  it("returns the expected map for one wire", () => {
    const wirePath = parseLineIntoWirePath("R8,U5,L5,D3");
    const result = convertWirePathsIntoMapStr([wirePath]);

    expect(result).toEqual(`...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........`);
  });

  it("returns the expected map for one wire (2)", () => {
    const wirePath = parseLineIntoWirePath("R8,U5,L5,D6,L4");
    const result = convertWirePathsIntoMapStr([wirePath]);

    expect(result).toEqual(`............
.....+----+.
.....|....|.
.....|....|.
.....|....|.
.....|....|.
..o--|----+.
.----+......
............`);
  });

  it("returns the expected map for two wires", () => {
    expect(
      convertWirePathsIntoMapStr([
        parseLineIntoWirePath("R8,U5,L5,D3"),
        parseLineIntoWirePath("U7,R6,D4,L4")
      ])
    ).toEqual(`...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........`);
  });
});

describe("getManhattanDistanceOfClosestCrossing", () => {
  it("returns the examples results", () => {
    expect(
      getManhattanDistanceOfClosestCrossing(
        parseLineIntoWirePath("R8,U5,L5,D3"),
        parseLineIntoWirePath("U7,R6,D4,L4")
      )
    ).toEqual(6);

    expect(
      getManhattanDistanceOfClosestCrossing(
        parseLineIntoWirePath("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
        parseLineIntoWirePath("U62,R66,U55,R34,D71,R55,D58,R83")
      )
    ).toEqual(159);

    expect(
      getManhattanDistanceOfClosestCrossing(
        parseLineIntoWirePath("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
        parseLineIntoWirePath("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
      )
    ).toEqual(135);
  });
});

describe("getFewestCombinedStepsOfCrossing", () => {
  it("returns the examples results", () => {
    expect(
      getFewestCombinedStepsOfCrossing(
        parseLineIntoWirePath("R8,U5,L5,D3"),
        parseLineIntoWirePath("U7,R6,D4,L4")
      )
    ).toEqual(30);

    expect(
      getFewestCombinedStepsOfCrossing(
        parseLineIntoWirePath("R75,D30,R83,U83,L12,D49,R71,U7,L72"),
        parseLineIntoWirePath("U62,R66,U55,R34,D71,R55,D58,R83")
      )
    ).toEqual(610);

    expect(
      getFewestCombinedStepsOfCrossing(
        parseLineIntoWirePath("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"),
        parseLineIntoWirePath("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
      )
    ).toEqual(410);
  });
});

describe("_test", () => {
  describe("serializePosition", () => {
    it("returns the expected format", () => {
      expect(serializePosition([-1, 3])).toEqual("-1_3");
    });
  });

  describe("deserializePosition", () => {
    it("returns the expected position", () => {
      expect(deserializePosition("-1_3")).toEqual([-1, 3]);
    });
  });
});
