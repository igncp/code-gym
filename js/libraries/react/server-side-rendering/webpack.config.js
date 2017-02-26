/* eslint-disable import/no-commonjs */

const loaders = [{
  loader: "babel-loader",
  test: /\.js$/,
}, {
  loader: "json-loader",
  test: /\.json$/,
}];

const entry = "./src/client/index.js";
const output = {
  filename: "bundle.js",
  path: `${__dirname}/build`,
};

module.exports = {
  devtool: "eval",
  entry,
  module: { loaders },
  output,
};
