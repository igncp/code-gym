import { expect } from "chai"

import R from "ramda"
import RF from "ramda-fantasy"

const checkEqlSpread = ({ fn, expected, initSpread }) => {
  const actual = fn(...initSpread)

  expect(actual).to.eql(expected)
}

const checkEql = ({ init, fn, expected }) => {
  checkEqlSpread({ initSpread: [init], fn, expected })
}

describe("sandbox", () => {
  it("1", () => {
    const initSpread = [3, 4, 9, -3]

    const { juxt, unapply } = R

    const fn = unapply(juxt)(Math.min, Math.max)
    const expected = [-3, 9]

    checkEqlSpread({ fn, expected, initSpread })
  })

  it("2", () => {
    const { is, always, of, over, inc, lensIndex, ifElse } = R

    const ifElseIsArray = ifElse(is(Array))

    const incrementHead = ifElseIsArray(
      over(lensIndex(0), inc),
      always(of(1))
    )

    const actual1 = incrementHead({ foo: "bar" })
    const expected1 = [1]
    expect(actual1).to.eql(expected1)

    const actual2 = incrementHead([1, 2])
    const expected2 = [2, 2]
    expect(actual2).to.eql(expected2)
  })

  it("3", () => {
    const init = "foobar"
    const expected = "FoObAr"

    const {
      invoker, addIndex, map, complement, modulo, compose,
      ifElse, toUpper, identity, nthArg, __,
    } = R

    // split :: String → [String]
    const split = invoker(1, "split")("")
    // join :: [String] → String
    const join = invoker(1, "join")("")
    // mapIndexed :: Functor f => (a, int → b) → f a → f b
    const mapIndexed = addIndex(map)
    // isEven :: Number → Boolean
    const isEven = complement(modulo(__, 2))
    // secondArgumentIsEven :: a → b → Number
    const secondArgumentIsEven = compose(isEven, nthArg(1))
    // upperOdd :: [String] → [String]
    const upperOdd = mapIndexed(ifElse(secondArgumentIsEven, toUpper, identity))
    // fn :: String → String
    const fn = compose(join, upperOdd, split)

    checkEql({ init, fn, expected })
  })

  it("4", () => {
    const {
      always, ifElse, compose, equals, modulo, __, unapply,
      apply, over, lensIndex, uncurryN, identity, curryN, converge,
    } = R


    // isInt :: Number → Boolean
    const isInt = compose(equals(0), modulo(__, 1))
    // isInt :: [(=> a), (=> b)] → a|b
    const applyIfElseIsInt = apply(ifElse(isInt))
    // isInt :: [a, b] → [a, b]
    const callAlwaysOnSecondArg = over(lensIndex(1), always)
    // recursivelyCallIfInt :: ((Number => Number), Number) → Number → Number
    const recursivelyCallIfInt = curryN(2, unapply(compose(
      applyIfElseIsInt,
      callAlwaysOnSecondArg
    )))

    const fn = num => converge(
      uncurryN(2, recursivelyCallIfInt(fn)),
      [identity, Math.sqrt]
    )(num)

    // minimum int with recursive sqrt
    checkEql({ init: 81, fn, expected: 3 })
    checkEql({ init: 16, fn, expected: 2 })
  })

  it("5", () => {
    const {
      converge, useWith, split, join, append, flip, compose, call, prepend, prop,
      times, always, maxBy, identity, reduce, reverse,
    } = R
    const init = "Foo foobar bar"
    const expected = `
******
Foo
foobar
bar
******
`
    const splitter = split(" ")
    const joiner = join("\n")
    const appendF = flip(append)
    const prependF = flip(prepend)
    const appendOrPrependToStrFn = compose(
      useWith(compose(join(""), call)),
      compose(
        reverse,
        appendF([split("")])
      )
    )

    const appendStr = appendOrPrependToStrFn(append)
    const prependStr = appendOrPrependToStrFn(prepend)
    const maxLengthOfStrings = compose(
      prop("length"),
      reduce(maxBy(prop("length")), "")
    )
    const createAsterisksStr = compose(
      join(""),
      times(always("*"))
    )
    const appendAsterisks = useWith(
      call,
      [appendF, createAsterisksStr]
    )
    const prependAsterisks = useWith(
      call,
      [prependF, createAsterisksStr]
    )

    const updateAppendingAsterisks = converge(appendAsterisks, [identity, maxLengthOfStrings])
    const updatePrependingAsterisks = converge(prependAsterisks, [identity, maxLengthOfStrings])

    const fn = compose(
      appendStr("\n"),
      prependStr("\n"),
      joiner,
      updateAppendingAsterisks,
      updatePrependingAsterisks,
      splitter
    )

    checkEql({ init, fn, expected })
  })

  it("6", () => {
    const { Maybe } = RF
    const {
      split, curryN, compose, call, join, append, composeK,
      unapply, apply, map,
    } = R

    const init = Maybe.Just("foo")
    const expected = Maybe.Just("f_o_o_")

    const justify = curryN(2, compose(Maybe.Just, call))
    const composeJust = unapply(compose(apply(composeK), map(justify)))

    const fn = composeJust(
      join("_"),
      append(""),
      split("")
    )

    checkEql({ init, fn, expected })
  })
})
