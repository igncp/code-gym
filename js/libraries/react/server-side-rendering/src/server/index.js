import express from "express";
import React from "react";
import { renderToString } from "react-dom/server";

import { App } from "../client/App";

const app = express();

app.use("/build", express.static("build"));

const getRandomInt = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

app.get("*", (req, res) => {
  res.setHeader("Content-Type", "text/html; charset=utf-8");

  const randomInt = getRandomInt(1, 10);
  const appInitialState = { start: randomInt };
  const elem = <App {...appInitialState} />;
  const html = `
  <html>
  <head>
  </head>
  <body>
  <div id="main">${renderToString(elem)}</div>
  <script>
    window.appInitialState = ${JSON.stringify(appInitialState)}
  </script>
  <script src="/build/bundle.js"></script>
  </body>
  </html>
  `;

  res.send(html);
});

app.listen(9000, () => {
  console.log("Listening on port 9000");
});
