import { expect } from "chai"
import R from "ramda"


describe("sandbox", () => {
  it("1", () => {
    const { juxt, unapply } = R

    const range = unapply(juxt)(Math.min, Math.max)
    const actual = range(3, 4, 9, -3)
    const expected = [-3, 9]

    expect(actual).to.eql(expected)
  })
})
