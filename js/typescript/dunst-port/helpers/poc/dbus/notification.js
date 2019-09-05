// https://pythonhosted.org/txdbus/dbus_overview.html

// https://dbus.freedesktop.org/doc/dbus-tutorial.html#whatis
// https://github.com/Shouqun/node-dbus
// Check example: less node_modules/dbus/examples

// GUI to inspect DBus: https://wiki.gnome.org/action/show/Apps/DFeet?action=show&redirect=DFeet#Releases

// https://developer.gnome.org/notification-spec/


const DBus = require("dbus");

const createNotification = async () => {
  const bus = DBus.getBus("session");

  return new Promise(resolve => {
    bus.getInterface(
      "org.freedesktop.Notifications",
      "/org/freedesktop/Notifications",
      "org.freedesktop.Notifications",
      (err, iface) => {
        const timeout = 2000;

        iface.Notify(
          "Application Name",
          {},
          {},
          "Message Title",
          "Message <b>Content</b>",
          [{}],
          [{}],
          timeout,
          () => {
            bus.disconnect();

            resolve();
          }
        );
      }
    );
  });
};

const main = async () => {
  await createNotification();
};

main();
