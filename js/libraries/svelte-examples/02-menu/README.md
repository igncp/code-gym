# 02 Menu

This example shows how to:

- Create a "Menu" component
    - Use lifecycle methods like `oncreate`
    - Communicate between components via `props`
    - Use computed data using `props` via the `computed` property
    - Call functions from the parent component via `props`
    - Add dynamic CSS classes
    - Use the templating system: e.g. loops
    - Add all the functionality in a single component: `Menu.html`
    - Learnt how the Scoped Styles work:
        - Not truly scoped. E.g. `.menu.svelte-123`: If a external stylesheet with `.menu`, it would pick it.
    - Used `immutable: true` to allow strict checks and improve performance: https://svelte.dev/docs#svelte_options
- Use Prettier to format HTML files, and use ESLint after for extra linting
    - Needs to use a special parser for HTML files to prevent changing elements case

Couldn't do yet:

- Configure prettier / ESLint to correctly format the templating system

Extra:

- Learn to use this website to find more performant CSS properties: https://csstriggers.com/
