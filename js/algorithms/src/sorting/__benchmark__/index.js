const Benchmark = require("benchmark")

const insertionSort = require("../insertion-sort")
const bubbleSort = require("../bubble-sort")

const suite = new Benchmark.Suite

const defaultArr = []

for (let i = 10000; i > 0; i--) {

  defaultArr.push(i)

}
const getArr = () => defaultArr.slice(0)

console.log("starting sorting benchmark")

suite.add("insert sort", function() {

  insertionSort(getArr())

})
.add("bubble sort", function() {

  bubbleSort(getArr())

})
.on("cycle", function(event) {

  console.log(String(event.target))

})
.on("complete", function() {

  console.log(`Fastest is ${this.filter("fastest").map("name")}`)

})
.run({"async": true})
