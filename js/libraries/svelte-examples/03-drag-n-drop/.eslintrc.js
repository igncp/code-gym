module.exports = {
  'env': {
    'browser': true,
    'es6': true,
    'node': true,
  },
  'plugins': [
    'svelte3',
  ],
  'extends': 'google',
  'globals': {
    'Atomics': 'readonly',
    'SharedArrayBuffer': 'readonly',
  },
  'parserOptions': {
    'ecmaVersion': 2018,
    'sourceType': 'module',
  },
  'rules': {
    'no-invalid-this': 0,
    'require-jsdoc': 0,
  },
};
