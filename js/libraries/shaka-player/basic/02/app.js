function _printMethodsAndProps(obj, filterPredicate) {
  const props = Object.keys(obj).filter(filterPredicate)
  const methods = Object.keys(obj.__proto__ || {}).filter(filterPredicate)

  console.info('Methods')
  console.info(JSON.stringify(methods.sort(), null, 4))
  console.info('Props')
  console.info(JSON.stringify(props.sort(), null, 4))
}

function printAllMethodsAndProps(obj) {
  _printMethodsAndProps(obj, () => true)
}

function printPublicMethodsAndProps(obj) {
  _printMethodsAndProps(obj, (k) => k[k.length - 1] !== '_')
}

function _logAndExpose(message, varName, varValue) {
  if (arguments.length === 2) {
    varValue = varName
    varName = message
  }

  if (varName === message) {
    console.info(message)
  } else {
    console.info(varName + ': ' + message)
  }

  window[varName] = varValue
}

function initApp() {
  shaka.polyfill.installAll();

  var video = document.getElementById('video');
  var player = new shaka.Player(video);

  console.info('--------------------')
  console.info('** printAllMethodsAndProps')
  console.info('** printPublicMethodsAndProps')
  console.info('')

  _logAndExpose('shaka', shaka);
  _logAndExpose('shaka.media', 'media', shaka.media);
  _logAndExpose('player', player);
  _logAndExpose('player.getNetworkingEngine()', 'networkingEngine', player.getNetworkingEngine());
  _logAndExpose('new shaka.text.Cue(0, 10)', 'cue', new shaka.text.Cue(0, 10));
  _logAndExpose('new shaka.dash.DashParser()', 'dashParser', new shaka.dash.DashParser());
  console.info('')
}

document.addEventListener('DOMContentLoaded', initApp);
