# `react-redux`

https://github.com/DefinitelyTyped/DefinitelyTyped/tree/363cdf403a74e0372e87bbcd15eb1668f4c5230b/types/react-redux

- For different versions, multiple directories are created
- The tests consist on normal functions using TSX
- Learn about module augmentation: http://www.typescriptlang.org/docs/handbook/declaration-merging.html#module-augmentation
- Learn about conditional types (including the predefined ones): https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-8.html
- Learn about `keyof` in types: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html
    - Also learnt about the tricky `infer` keyword
- Learn about the `never` keyword: https://basarat.gitbooks.io/typescript/docs/types/never.html

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
      - Shared
    - TInjectedProps
  - DispatchProp
```
