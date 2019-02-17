# `react-redux`

https://github.com/DefinitelyTyped/DefinitelyTyped/tree/363cdf403a74e0372e87bbcd15eb1668f4c5230b/types/react-redux

- For different versions, multiple directories are created
- The tests consist on normal functions using TSX
- Learn about module augmentation: http://www.typescriptlang.org/docs/handbook/declaration-merging.html#module-augmentation
- Learn about conditional types (including the predefined ones): https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html
- Learn about `keyof` in types: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html
    - Also learnt about the tricky `infer` keyword
- Learn about the `never` keyword: https://basarat.gitbooks.io/typescript/docs/types/never.html
- Uses the conditional type expression to resolve thunks if they are functions (checking ` extends (...args: any[]) => any`)
- Learn about the state of JSX in TS: https://www.typescriptlang.org/docs/handbook/jsx.html
    - Found where the namespace is defined: https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/react/index.d.ts#L2735
    - Also learnt that this is in the `global` namespace

## Examples

- Connect with no arguments

```
interface Connect:
  - InferableComponentEnhancer
    - InferableComponentEnhancerWithProps
      - ComponentType
      - Matching
      - GetProps
      - ConnectedComponentClass
      - Omit
        - Uses Pick and Exclude advanced types
      - Shared
    - TInjectedProps
  - DispatchProp
```
