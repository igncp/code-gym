const {
  READ_INPUT,
  CLEAR_STATE,
} = require("./actions")

const initialState = {content: null}

const rootReducer = function(state = initialState, {type, data}) {
  switch (type) {
  case CLEAR_STATE:
    return initialState
  case READ_INPUT:
    return Object.assign(state, {content: data.content})
  default:
    return state
  }
}

module.exports = {
  initialState,
  rootReducer,
}
