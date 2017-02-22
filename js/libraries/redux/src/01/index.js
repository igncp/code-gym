const {createStore} = require("redux")

const {readInput} = require("./actions")
const {rootReducer} = require("./reducers")

const store = createStore(rootReducer)

const input = process.argv[2]

store.dispatch(readInput(input))
