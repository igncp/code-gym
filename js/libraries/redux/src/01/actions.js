const READ_INPUT = "READ_INPUT"
const CLEAR_STATE = "CLEAR_STATE"

function readInput(content) {
  return {
    data: {content},
    type: READ_INPUT,
  }
}

function clearState() {
  return {type: CLEAR_STATE}
}

module.exports = {
  CLEAR_STATE,
  READ_INPUT,

  clearState,
  readInput,
}
