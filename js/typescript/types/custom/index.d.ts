// --- start (1)

// These examples are from: https://www.typescriptlang.org/docs/handbook/advanced-types.html
// They have some modifications

export declare const pluck: <T, K extends keyof T>(obj: T, names: K[]) => T[K][];

type Proxy<T> = {
  get_val(): T;
  get(): void;
  set(value: T): void;
}

type Proxify<T> = {
  [P in keyof T]: Proxy<T[P]>;
}

export declare const proxify: <T>(o: T) => Proxify<T>;

export declare const getWithDynamicType: <T extends number>(x: T) => T extends (1 | 2) ? string : number;

// --- end (1)
