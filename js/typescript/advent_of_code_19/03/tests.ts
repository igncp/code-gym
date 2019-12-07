import {
  WireDirection,
  parseLineIntoWirePath,
  convertWirePathsIntoMapStr
} from "./lib";

describe("parseLineIntoWirePath", () => {
  it("returns the expected result", () => {
    expect(parseLineIntoWirePath("R8,U5")).toEqual([
      [WireDirection.Right, 8],
      [WireDirection.Up, 5]
    ]);
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

  it("returns the expected map for two wires", () => {
    const wirePathA = parseLineIntoWirePath("R8,U5,L5,D3");
    const wirePathB = parseLineIntoWirePath("U7,R6,D4,L4");
    const result = convertWirePathsIntoMapStr([wirePathA, wirePathB]);
    expect(result).toEqual(`...........
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
