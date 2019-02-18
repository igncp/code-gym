import {
  pluck,
  proxify,
  getWithDynamicType,
  getArray,
  getFirstThree
} from '.';

// --- start (1)
// https://www.typescriptlang.org/docs/handbook/advanced-types.html

interface Person {
  age: number;
  height?: number;
  name: string;
}
const person: Person = {
  age: 35,
  name: 'A Name'
};

const names: string[] = pluck(person, ['name']);
// const surnames: string[] = pluck(person, ['surname']); // ERROR: property not present
// const namesNums: number[] = pluck(person, ['name']); // ERROR: property has wrong type

const proxiedPerson = proxify(person);
const name: string = proxiedPerson.name.get_val();
// const name2: string = proxiedPerson.name; // ERROR: string is not the same as Proxy
// proxiedPerson.name = 'foo'; // ERROR: property is not assignable to Proxy type

const oneVal: string = getWithDynamicType(1);
// const oneVal_num: number = getWithDynamicType(1); // ERROR: number is not expected when 1
const threeVal: number = getWithDynamicType(3);
// const threeVal_str: string = getWithDynamicType(3); // ERROR: string is not expected when 3

const resultFirstThree: 1 = getFirstThree(1);
// const resultFirstThree2: 5 = getFirstThree(4); // ERROR: null is not assignable to 5

const extracted1: Extract<1 | 2, number> = 1;
// const extracted2: Extract<1 | 2, string> = 1;  // ERROR: 1 is not assignable to 'never'

const numsArray: number[] = getArray(1);
const numsArray2: string[] = getArray('a');
// const numsArray3: string[] = getArray([1]); // ERROR: void is not assignable to type `string[]`
// const numsArray4: string[] = getArray(['a']); // ERROR: void is not assignable to type `string[]`

// --- end (1)
