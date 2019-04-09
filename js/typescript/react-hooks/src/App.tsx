import React, { Component } from 'react';
import { ReactElement } from 'react';
import logo from './logo.svg';
import './App.css';

import { useBattery, useSize } from 'react-use';
import { BatterySensorState } from 'react-use/esm/useBattery';

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
          <BatteryDemo />
          <SizeDemo />
        </main>
      </div>
    );
  }
}

export default App;
