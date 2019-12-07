enum WireDirection {
  Down = "Down",
  Left = "Left",
  Right = "Right",
  Up = "Up"
}

type WireSegment = [WireDirection, number];

type WirePath = WireSegment[];

type GetDirectionFromChar = (ch: string) => WireDirection;

const getDirectionFromChar: GetDirectionFromChar = ch => {
  switch (ch.toUpperCase()) {
    case "U":
      return WireDirection.Up;
    case "D":
      return WireDirection.Down;
    case "L":
      return WireDirection.Left;
    case "R":
      return WireDirection.Right;
  }

  throw new Error("Invalid Char");
};

type ParseLineIntoWirePath = (line: string) => WirePath;

const parseLineIntoWirePath: ParseLineIntoWirePath = line => {
  return line
    .split(",")
    .filter(i => !!i)
    .map(segmentStr => {
      const direction = getDirectionFromChar(segmentStr[0]);
      const num = Number(segmentStr.slice(1, segmentStr.length));

      return [direction, num];
    });
};

// x,y
type Position = [number, number];

type GetNewPositionAfterSegment = (p: Position, s: WireSegment) => Position;

const getNewPositionAfterSegment: GetNewPositionAfterSegment = (p, s) => {
  const newPosition = p.slice(0);

  switch (s[0]) {
    case WireDirection.Down:
      newPosition[1] -= s[1];
      break;
    case WireDirection.Up:
      newPosition[1] += s[1];
      break;
    case WireDirection.Left:
      newPosition[0] -= s[1];
      break;
    case WireDirection.Right:
      newPosition[0] += s[1];
      break;
  }

  return [newPosition[0], newPosition[1]];
};

interface WirePathBoundary {
  maxX: number;
  maxY: number;
  minX: number;
  minY: number;
}

type GetWirePathBoundary = (p: WirePath) => WirePathBoundary;

const getWirePathBoundary: GetWirePathBoundary = p => {
  const maxMinX = [0, 0];
  const maxMinY = [0, 0];
  let pos: Position = [0, 0];

  for (let stepNum = 0; stepNum < p.length; stepNum += 1) {
    const step = p[stepNum];

    pos = getNewPositionAfterSegment(pos, step);

    if (pos[0] < maxMinX[1]) {
      maxMinX[1] = pos[0];
    } else if (pos[0] > maxMinX[0]) {
      maxMinX[0] = pos[0];
    }

    if (pos[1] < maxMinY[1]) {
      maxMinY[1] = pos[1];
    } else if (pos[1] > maxMinY[0]) {
      maxMinY[0] = pos[1];
    }
  }

  return {
    maxX: maxMinX[0],
    maxY: maxMinY[0],
    minY: maxMinY[1],
    minX: maxMinX[1]
  };
};

type ConvertWirePathsIntoMapStr = (paths: WirePath[]) => string;

const convertWirePathsIntoMapStr: ConvertWirePathsIntoMapStr = paths => {
  const pathBoundaries = paths.map(p => getWirePathBoundary(p));
  const map: string[][] = [];

  const { maxX, minX, maxY, minY } = pathBoundaries.reduce(
    (
      acc: WirePathBoundary,
      pathBoundary: WirePathBoundary
    ): WirePathBoundary => {
      return {
        maxX: Math.max(acc.maxX, pathBoundary.maxX),
        minX: Math.min(acc.minX, pathBoundary.minX),
        maxY: Math.max(acc.maxY, pathBoundary.maxY),
        minY: Math.min(acc.minY, pathBoundary.minY)
      };
    },
    { maxX: 0, minX: 0, maxY: 0, minY: 0 }
  );

  for (let y = maxY + 1; y > minY - 2; y -= 1) {
    const line = [];

    for (let x = minX - 1; x < maxX + 2; x += 1) {
      line.push(".");
    }

    map.push(line);
  }

  paths.forEach(path => {
    const prevMap = map.slice(0).map(l => l.slice(0));
    const pos: Position = [0, 0];

    const updateMapWithChar = (ch: string) => {
      const y = maxY - pos[1] + 1;
      const x = pos[0] - minX + 1;

      map[y][x] = prevMap[y][x] === "." || ch === "o" ? ch : "X";
    };

    updateMapWithChar("o");

    for (let stepNum = 0; stepNum < path.length; stepNum += 1) {
      const step = path[stepNum];

      if (pos[0] !== 0 || pos[1] !== 0) {
        updateMapWithChar("+");
      }

      const initX = pos[0];
      const initY = pos[1];

      if (step[0] === WireDirection.Down) {
        for (let y = pos[1]; y > initY - step[1]; y -= 1) {
          pos[1] -= 1;
          updateMapWithChar("|");
        }
      } else if (step[0] === WireDirection.Up) {
        for (let y = pos[1]; y < initY + step[1]; y += 1) {
          pos[1] += 1;
          updateMapWithChar("|");
        }
      }
      if (step[0] === WireDirection.Left) {
        for (let x = pos[0]; x > initX - step[1]; x -= 1) {
          pos[0] -= 1;
          updateMapWithChar("-");
        }
      } else if (step[0] === WireDirection.Right) {
        for (let x = pos[0]; x < initX + step[1]; x += 1) {
          pos[0] += 1;
          updateMapWithChar("-");
        }
      }
    }
  });

  return map.map(line => line.join("")).join("\n");
};

export {
  WireDirection,
  WirePath,
  convertWirePathsIntoMapStr,
  parseLineIntoWirePath
};
