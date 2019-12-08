module.exports = {
  preset: "ts-jest",
  testMatch: ["**/tests.ts"],
  globals: {
    __TEST__: true
  },
  coverageThreshold: {
    global: {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    }
  }
};
