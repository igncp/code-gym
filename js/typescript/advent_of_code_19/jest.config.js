module.exports = {
  preset: "ts-jest",
  testMatch: ["**/tests.ts"],
  coverageThreshold: {
    global: {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    }
  }
};
