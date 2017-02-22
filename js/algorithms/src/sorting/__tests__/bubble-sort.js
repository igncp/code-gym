const {expect} = require("chai")

const bubbleSort = require("../bubble-sort")

describe("insertion sort", () => {

  it("returns the sorted array", () => {

    expect(bubbleSort([3, 2, 1])).to.eql([1, 2, 3])

  })

  it("sorts by reference", () => {

    const arr = [5, 2, 4]

    bubbleSort(arr)
    expect(arr).to.eql([2, 4, 5])

  })

})
