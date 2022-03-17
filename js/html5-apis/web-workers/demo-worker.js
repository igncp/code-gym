importScripts("https://cdnjs.cloudflare.com/ajax/libs/redux/4.1.2/redux.js");

// https://developer.mozilla.org/en-US/docs/Web/API/Worker

const { createStore, applyMiddleware } = Redux;

const logger = (store) => (next) => (action) => {
  next(action);

  const { count } = store.getState();

  if (count % 2 === 0) {
    console.log("even count: " + count);
  }
};

const main = () => {
  const reducer = (state, action) => {
    if (action.type === "INCREMENT") {
      return {
        ...state,
        count: state.count + 1,
      };
    }
    return state;
  };

  const store = createStore(
    reducer,
    {
      count: 0,
    },
    applyMiddleware(logger)
  );

  onmessage = (e) => {
    const { data: action } = e;
    store.dispatch(action);
    postMessage(store.getState());
  };
};

main();
