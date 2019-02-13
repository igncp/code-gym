import {
  pluck,
  proxify,
  getWithDynamicType
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

// --- end (1)
