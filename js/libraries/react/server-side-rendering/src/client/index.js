import React from "react";
import { render } from "react-dom";

import { App } from "./App";

const main = document.getElementById("main");
const appInitialState = window.appInitialState;

render(<App {...appInitialState} />, main);
