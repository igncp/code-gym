# Examples

## ReactiveX/rxjs
Run mocha tests:
- `npm i && npm build_test`
- then, on each change ... `npm run build_spec && npm run test_mocha`
- that runs all the tests, to run one, for example use: `find spec | entr sh -c "npm run build_spec && ./node_modules/.bin/mocha --opts spec/support/default.opts spec-js/Observable-spec.js"`
