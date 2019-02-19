module.exports = {
  includes: 'index.ts',
  mode: 'file',
  excludeNotExported: true,
  excludeExternals: true,
  excludePrivate: true,
  excludeProtected: true,
  out: './docs',
  target: 'ES6'
}
