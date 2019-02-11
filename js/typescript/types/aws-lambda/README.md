# aws-lambda

## `tsconfig.json`

List of folders to include type definitions from. It allows to import external types

```
"typeRoots": [
    "../"
],
```

This is set to false by default.

```
"forceConsistentCasingInFileNames": true
```

The test runner used is the types-publisher tester:
https://github.com/Microsoft/types-publisher/blob/master/src/tester/test.ts

```
  "aws-lambda-tests.ts"
```

## `tslint.json`

There is a common config for all the types, named `dtslint`

## `index.d.ts`

- Single implementation file.
- There are several `// @TODO` lines.
- The last line is using `export as namespace AWSLambda`
- Doesn't have many dynamic types, some examples: `Callback`, `Handler`
- Uses typedoc with the `@param NAME - DESCRIPTION` format
- When using the `|` in types in multiple lines, it is possible to start with `|` the first item, that way is easier to sort and add (although last item has a `;`)
- It mostly uses `interface` except for type aliases
- The `string | string[]` type is common
- It uses the `non-null` assertion operator:

```
A new ! post-fix expression operator may be used to assert that its operand is
non-null and non-undefined in contexts where the type checker is unable to
conclude that fact. Specifically, the operation x! produces a value of the type
of x with null and undefined
```

## `aws-lambda-tests.ts`

Tests are run in parallel. The way tests work is by defining variables and
types.

## [types](..)
