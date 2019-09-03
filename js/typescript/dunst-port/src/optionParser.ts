import parse from "yargs-parser";

const argv = parse(process.argv);

const getRequestedVersion = (): boolean => {
  return argv.version;
};

const getRequestedVerbosity = (): string => {
  return argv.verbosity || "";
};

export { getRequestedVersion, getRequestedVerbosity };
