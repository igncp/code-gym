const {expect} = require("chai")

const {
  clearState,
  CLEAR_STATE,
  READ_INPUT,
  readInput,
} = require("./actions")
const {
  rootReducer,
} = require("./reducers")

describe("01", () => {
  describe("actions", () => {
    it("readInput returns expected", () => {
      const value = Math.random()

      expect(readInput(value)).to.eql({
        data: {content: value},
        type: READ_INPUT,
      })
    })

    it("clearState returns expected", () => {
      expect(clearState()).to.eql({type: CLEAR_STATE})
    })
  })

  describe("reducers", () => {
    describe("rootReducer", () => {
      it("returns expected default", () => {
        const value = Math.random()

        expect(rootReducer(value, {type: null})).to.eql(value)
      })
    })
  })
})
