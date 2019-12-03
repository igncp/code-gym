type GetFuelForModuleMass = (mass: number, includeFuel?: boolean) => number;

export const getFuelForMass: GetFuelForModuleMass = (
  mass,
  includeFuel = false
) => {
  const fuel = Math.floor(mass / 3) - 2;

  if (fuel < 0) {
    return 0;
  }

  if (includeFuel) {
    return fuel + getFuelForMass(fuel, includeFuel);
  }

  return fuel;
};

type GetFuelForModules = (modules: number[], includeFuel?: boolean) => number;

export const getFuelForModules: GetFuelForModules = (
  modules,
  includeFuel = false
) => {
  return modules.reduce(
    (acc, moduleMass) => acc + getFuelForMass(moduleMass, includeFuel),
    0
  );
};

type ConvertFileContentIntoModules = (content: string) => number[];

export const convertFileContentIntoModules: ConvertFileContentIntoModules = fileContent => {
  return fileContent
    .split("\n")
    .filter(l => !!l)
    .map(l => Number(l));
};
