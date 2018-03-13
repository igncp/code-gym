const main = () => {
  var hidden, visibilityChange;

  const handleVisibilityChange = (el) => {
    console.log('el', el);
    console.log('hidden', hidden);
    console.log('document[hidden]', document[hidden]);
    console.log('document.visibilityState', document.visibilityState);
  }

  if (typeof document.hidden !== "undefined") {
    hidden = "hidden";
    visibilityChange = "visibilitychange";
  } else if (typeof document.msHidden !== "undefined") {
    hidden = "msHidden";
    visibilityChange = "msvisibilitychange";
  } else if (typeof document.webkitHidden !== "undefined") {
    hidden = "webkitHidden";
    visibilityChange = "webkitvisibilitychange";
  }

  document.addEventListener(visibilityChange, handleVisibilityChange, false);
}

main()
