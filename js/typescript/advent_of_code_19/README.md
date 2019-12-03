# Advent Of Code 19

- https://adventofcode.com/

## List of Completed

- [01](./01)

## Objectives

These may change in future, but all examples should comply with them:

- All in TypeScript
- Consistent files:
  - `main.ts`, `lib.ts`, `test.ts`, `README.md`, `input.txt`
  - `package.json`, `package-lock.json`, `tsconfig.json`
- 100% type coverage for `lib.ts`
- 100% line coverage for `lib.ts`
- TDD
- Different approaches:
    - Indicated in each exercise
    - Example:Functional programming, object oriented, data driven
- Minimize dependencies (listed all here):
    - Language: `typescript`, `ts-node`
    - Test: Jest
    - Lint: ESLint, Prettier
- Scripts:
    - `npm run check`: Linting, Prettier and type coverage
    - `npm test`: Unit tests for `lib.ts`
    - `npm run fix`: ESLint and Prettier fixer
    - `npm start`: Run `main.ts` with my input
- All examples should have same:
    - Scripts
    - ESLint and Prettier config
    - Format for output
- Examples can have different:
    - Unit tests framworks

## TODO:

- Move all `node_modules` into root
- Update dependencies
- Add scripts from top-level
- Setup prettier
- Setup ESLint
- Move all configs to root level (except maybe `tsconfig.json`)
- Setup types and tests coverages limits
- Periodic: Refactor interfaces
