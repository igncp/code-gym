# React 16.3 tests

In this `README.md` it will describe the features that have been tested, the patterns learnt and any other extra interesting references. To see about the process, check out [the log](./log.md).

Not all the tests are specific to v16.3 but to 16 in general, it just happens that 16.3 is the latest version at the moment of writing 

Features:

**v16**

- [Fragments](https://reactjs.org/docs/fragments.html): See [CompWithFragments](./src/CompWithFragments.js)
- [Error Boundaries](https://reactjs.org/blog/2017/07/26/error-handling-in-react-16.html): Which uses `componentDidCatch`. See [ErrorBoundary](./src/ErrorBoundary.js)
- [Portals](https://reactjs.org/docs/portals.html): Which has a better API to render elements in different DOM nodes. See [CompWithPortal](./src/CompWithPortal.js)

**v16.3**

- [getDerivedStateFromProps](https://reactjs.org/blog/2018/03/27/update-on-async-rendering.html#new-lifecycle-getderivedstatefromprops): Used the new lifecycle method in [DerivedStateComp](./src/DerivedStateComp.js)
- [New Context API](https://reactjs.org/docs/context.html): Created the [compsWithContext](./src/compsWithContext.js) where it uses both the `Consumer` and `Provider` from the new `createContext` function

Notes:

- For error boundaries, even if the error is caught by an error boundary element, it would be logged to the console. Also in `create-react-app` the development error overlay will still be displayed
- The gist of the new 16.3 API for lifecycle methods is to move side effects and asynchronous initialization methods from `componentWill*` methods (where `*` is `Mount` or `Update`) to the respective `componentDid*` methods, or to `getDerivedStateFromProps`. This helps performance, server-side rendering and the user-perception is the same
- For the new context API, it is important to understand what happens when a consumer is inside a provider (uses the provider value) or outside (uses the default value). It is also important to understand what happens when nesting providers (the deepest provider takes preference)
- Providers and Consumers can be top-level components inside a `render` function

Other:

- As this is a minimal Proof of Concept project, not adding any tooling like ESLint or Flowtype. Some ESLint errors are displayed in the browser's console.

---

This project was bootstrapped with [Create React App](https://github.com/facebookincubator/create-react-app).
