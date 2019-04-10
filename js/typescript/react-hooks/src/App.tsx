import React, { Component, useRef, useEffect, useState } from 'react';
import { ReactElement } from 'react';
import logo from './logo.svg';
import './App.css';

import { useBattery, useSize, useFullscreen, useToggle } from 'react-use';
import { BatterySensorState } from 'react-use/esm/useBattery';

// FROM: https://overreacted.io/making-setinterval-declarative-with-react-hooks
// And adding the types (Generics and !)
const useIntervalHook = <Callback extends () => void>(callback: Callback, delay?: number) => {
  const savedCallback = useRef<Callback>();

  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  useEffect(() => {
    const tick = () => {
      savedCallback.current!();
    };

    if (delay || delay !== 0) {
      let id = setInterval(tick, delay);
      return () => clearInterval(id);
    }
  }, [delay]);
};

const UseEffectDemo = () => {
  const [isBlue, setIsBlue] = useState(false);

  useIntervalHook(() => {
    setIsBlue(!isBlue);
  }, 1000)

  return (
    <React.Fragment>
      <h2>Use Effect Demo</h2>
      <p>This component uses the native "useEffect" to set the state within an interval</p>
      <div style={{ backgroundColor: isBlue ? 'blue' : 'red' }}>Content</div>
    </React.Fragment>
  )
}

const FullscreenDemo = () => {
  const [show, toggle] = useToggle(false);

  const ref = useRef(null);
  const onClose = useRef(() => toggle(false)).current;

  const isFullscreen = useFullscreen(ref, show, {onClose });

  return (
    <React.Fragment>
      <h2>Full Screen Demo</h2>
      <div ref={ref} style={{ backgroundColor: '#e0e0f8' }}>
        <p>The div in the blue box will turn full screen.</p>
        <button onClick={() => toggle()}>{isFullscreen ? 'Close' : 'Set full screen'}</button>
      </div>
    </React.Fragment>
  );
};

const BatteryDemo = () => {
  const state = useBattery() as BatterySensorState;

  // console.log(state.foo) // Correct TypeScript Error

  return (
    <React.Fragment>
      <h2>Battery Demo</h2>
      <p>This component uses "useBattery" which generates a JSON object which is updated when the battery changes.</p>
      <p>I had to install rebound and keyboardjs NPM packages.</p>
      <pre style={{ backgroundColor: '#eee' }}>
        {JSON.stringify(state, null, 2)}
      </pre>
    </React.Fragment>
  );
};

const SizeDemo = () => {
  const [sized, {width, height}]: [ReactElement<any>, { width: number, height: number }] = useSize(
    ({width}) => <div style={{border: '1px solid red'}}>Size me up! ({width}px)</div>
  );

  return (
    <React.Fragment>
      <h2>Size Demo</h2>
      <p>This component uses "useSize". It needs to receive a Component to be rendered.</p>
      <p>It is updated on resize.</p>
      {sized}
      <div>width: {width}</div>
      <div>height: {height}</div>
    </React.Fragment>
  );
};

class App extends Component {
  render() {
    return (
      <div className="App">
        <header>
          <p>React Hooks Examples</p>
          <p>Currently using. <a href="https://github.com/streamich/react-use">React Use</a></p>
          <p>It tries to use the as many static types (defined in <b>node_modules/react-use/esm/useBattery.d.ts</b>)</p>
        </header>
        <main style={{ textAlign: 'left' }}>
          <UseEffectDemo />
          <BatteryDemo />
          <FullscreenDemo />
          <SizeDemo />
        </main>
      </div>
    );
  }
}

export default App;
