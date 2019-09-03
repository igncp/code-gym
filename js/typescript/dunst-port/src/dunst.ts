import pjson from "../package.json";

import * as queues from "./queues";
import { logPlain, setLoggerLevel } from "./log";
import { getRequestedVerbosity, getRequestedVersion } from "./optionParser";

interface Status {
  fullscreen: boolean;
  idle: boolean;
  running: boolean;
}

const status: Status = {
  fullscreen: false,
  idle: false,
  running: false
};

const logVersionAndExit = () => {
  logPlain(
    `Dunst - A customizable and lightweight notification-daemon ${pjson.version}`
  );

  process.exit(0);
};

const main = () => {
  status.idle = false;
  status.running = true;

  queues.init();

  if (getRequestedVersion()) {
    logVersionAndExit();
  }

  const verbosity = getRequestedVerbosity();
  setLoggerLevel(verbosity || "info");

  //> load settings
  //> print help if necessary
  //> init dbus
  //> setup main loop
  //> setup signals
  //> setup startup notification
  //> run
  //> remove signal watches
  //> teardown gbus
};

export { main };
