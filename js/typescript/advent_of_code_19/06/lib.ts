type ObjectId = string;

type OrbitsMap = {
  [objectId: string]: {
    orbiting: ObjectId | null;
    orbitedBy: ObjectId[];
  };
};

type TransformStrIntoOrbitsMap = (str: string) => OrbitsMap;

const transformStrIntoOrbitsMap: TransformStrIntoOrbitsMap = str => {
  const orbitsMap: OrbitsMap = {};

  return str
    .split("\n")
    .filter(l => !!l)
    .reduce((acc, orbit) => {
      const [orbited, orbiting] = orbit.split(")");

      acc[orbiting] = acc[orbiting] || {
        orbiting: null,
        orbitedBy: []
      };

      if (acc[orbiting].orbiting) {
        throw new Error("Unexpected duplicate orbit: " + orbiting);
      }

      acc[orbiting].orbiting = orbited;

      acc[orbited] = acc[orbited] || {
        orbiting: null,
        orbitedBy: []
      };

      acc[orbited].orbitedBy.push(orbiting);

      return acc;
    }, orbitsMap);
};

type GetDirectAndInderctOrbitsNum = (o: string) => number;

const getDirectAndInderctOrbitsNum: GetDirectAndInderctOrbitsNum = str => {
  const orbitsMap = transformStrIntoOrbitsMap(str);

  type RecursiveFn = (objectId: string, carried: number) => number;

  const recursiveFn: RecursiveFn = (objectId, carried) => {
    const obj = orbitsMap[objectId];

    return obj.orbitedBy.reduce((acc, otherObjectId) => {
      return acc + recursiveFn(otherObjectId, carried + 1);
    }, carried);
  };

  return recursiveFn("COM", 0);
};

type GetAllOrbitedForObj = (objId: string, orbitsMap: OrbitsMap) => ObjectId[];

const getAllOrbitedForObj: GetAllOrbitedForObj = (objId, orbitsMap) => {
  const orbited: ObjectId[] = [];

  let currentObj: string | null = objId;

  while (currentObj) {
    orbited.push(currentObj);

    currentObj = orbitsMap[currentObj].orbiting;
  }

  return orbited.filter(o => o !== objId);
};

type GetMinimumOrbitTransfers = (
  str: string,
  items: [ObjectId, ObjectId]
) => number;

const getMinimumOrbitTransfers: GetMinimumOrbitTransfers = (
  str,
  [objectIdA, objectIdB]
) => {
  const orbitsMap = transformStrIntoOrbitsMap(str);

  const traceForA = getAllOrbitedForObj(objectIdA, orbitsMap);
  const traceForB = getAllOrbitedForObj(objectIdB, orbitsMap);

  type IndexMap = { [k: string]: number };
  const itemBToIdx: IndexMap = traceForB.reduce((acc: IndexMap, itemB, idx) => {
    acc[itemB] = idx + 1;

    return acc;
  }, {});

  let totalOrbits = traceForA.length - 1 + traceForB.length - 1;

  for (let itemAIdx = 0; itemAIdx < traceForA.length; itemAIdx += 1) {
    const itemA = traceForA[itemAIdx];

    if (itemBToIdx[itemA]) {
      totalOrbits = itemAIdx + (itemBToIdx[itemA] - 1);

      break;
    }
  }

  return totalOrbits;
};

type Test = null | {
  getAllOrbitedForObj: GetAllOrbitedForObj;
  transformStrIntoOrbitsMap: TransformStrIntoOrbitsMap;
};

let _test: Test = null;

// istanbul ignore else
if (typeof __TEST__ !== "undefined") {
  _test = {
    getAllOrbitedForObj,
    transformStrIntoOrbitsMap
  };
}

export { getDirectAndInderctOrbitsNum, getMinimumOrbitTransfers, _test };
