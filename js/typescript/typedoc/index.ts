/**
 * This is a file to study the support of Typedoc.
 *
 * Summary:
 * - Several tags are not well supported: `@deprecated`, `@see`
 * - The result for `@param` and `@returns` is different for type than for const
 */
export interface Summary {
  text: string;
}

/**
 * @param arr An array of numbers
 * @returns One number
 */
export type Foo = (arr: number[]) => number;

/**
 * This is a description for the type
 *
 * @returns Always one
 *
 * @example ```js
 * import {foo} from '.'
 * const bar = foo
 * ```
 * @deprecated
 *
 */
export const foo: Foo = arr => arr[0];
