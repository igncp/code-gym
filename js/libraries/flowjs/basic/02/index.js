// @flow

import _ from "lodash"

import type {
  Item,
  ItemPredicate,
  ItemLogger,
} from "./types"

const logItem: ItemLogger = (i: Item): void => console.log(i.value)
const items: Item[] = [{
  fn: (): number => 1,
  key: "FOO",
  value: "foo",
}, {
  fn: (): string => "a",
  key: "BAR",
  value: "bar",
}]
const isFoo: ItemPredicate = (i: Item): boolean => i.value === "foo"
const foo: Item = _.find(items, isFoo)

logItem(foo)
