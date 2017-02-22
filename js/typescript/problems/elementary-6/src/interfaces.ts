/// <reference path="../typings/modules/bluebird/index.d.ts" />

import * as Promise from "bluebird";

export type Requester = () => Promise<any>;

export type BootstrapFn = () => void;

export type Strategy = "sum" | "multiplication"
