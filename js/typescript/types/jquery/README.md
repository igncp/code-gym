# jQuery

- The `index.d.ts` file is divided in multiple files
- Learned about the `@see`, `@example`, `@deprecated` and `@since` JSDoc tags
- Learned about the `this` type: https://www.typescriptlang.org/docs/handbook/advanced-types.html (Polymorphic `this` types section)
- SizzleJS is jQuery's selector engine
- In the misc file the type: `type TypeOrArray<T> = T | T[];` is widely used
- Learned about Weak Types detection, the index signature workaround: https://mariusschulz.com/blog/typescript-2-4-weak-type-detection

> The takeaway here is that the heuristics behind weak type detection are designed to minimize the number of false positives (correct usages treated as incorrect), which comes at the expense of fewer true positives (incorrect usages treated as incorrect).

- Learned about numeric separators (e.g. `1_000_000`) similar than in Rust.
