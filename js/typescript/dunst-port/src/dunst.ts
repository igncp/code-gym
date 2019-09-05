import pjson from "../package.json";
import DBus from "dbus";
import ntk from "ntk";

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

const bus = DBus.getBus("session");

const logVersionAndExit = () => {
  logPlain(
    `Dunst - A customizable and lightweight notification-daemon ${pjson.version}`
  );

  process.exit(0);
};

const initDBus = () => {
  return new Promise(resolve => {
    bus.getInterface(
      "org.freedesktop.Notifications",
      "/org/freedesktop/Notifications",
      "org.freedesktop.Notifications",
      (err: any, iface: any) => {
        resolve(iface);
      }
    );
  });
};

const setNtkPoc = () => {
  return new Promise(resolve => {
    ntk.createClient((err: any, app: any) => {
      var mainWnd = app.createWindow({
        width: 500,
        height: 300,
        title: "Hello"
      });
      mainWnd.on("mousedown", (ev: any) => {
        mainWnd.setTitle("click: " + [ev.x, ev.y].join(","));
      });
      mainWnd.map();
      resolve();
    });
  });
};

const main = async () => {
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

  await initDBus();

  // await setNtkPoc();

  //> setup main loop
  //> setup signals
  //> setup startup notification
  //> run
  //> remove signal watches
  //> teardown gbus

  // bus.disconnect();
};

export { main };
