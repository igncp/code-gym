// @flow

export type Item = {|
  fn: () => number | string,
  key: string,
  value: string,
|}

export type ItemPredicate = (i: Item) => boolean
export type ItemLogger = (i: Item) => void
