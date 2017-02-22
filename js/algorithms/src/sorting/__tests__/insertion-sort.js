const {expect} = require("chai")

const insertionSort = require("../insertion-sort")

describe("insertion sort", () => {

  it("returns the sorted array", () => {

    expect(insertionSort([3, 2, 1])).to.eql([1, 2, 3])

  })

  it("sorts by reference", () => {

    const arr = [5, 2, 4]

    insertionSort(arr)
    expect(arr).to.eql([2, 4, 5])

  })

})
