import React, { Component } from 'react';

import logo from './logo.svg';
import './App.css';

import CompWithFragments from './CompWithFragments'
import { ErrorBoundary, CompThatErrors } from './ErrorBoundary'
import CompWithPortal from './CompWithPortal'
import DerivedStateComp from './DerivedStateComp'
import {CompContextProvider, CompContextConsumer} from './compsWithContext'

class App extends Component {
  constructor() {
    super()
    this.state = {}
  }

  componentWillMount() {
    this.timerId = setInterval(() => {
      this.setState(state => ({
        timerValue: state.timerValue + 1
      }))
    }, 3 * 1000)
  }

  componentWillUnmount() {
    clearInterval(this.timerId)
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <CompWithFragments />
        <ErrorBoundary>
          <CompThatErrors />
        </ErrorBoundary>
        <CompWithPortal />
        <DerivedStateComp timerValue={this.state.timerValue}/>
        <CompContextProvider>
          <div>
            {'Here is the context consumer:'}
            <CompContextConsumer isInsideProvider/>
          </div>
        </CompContextProvider>
        <CompContextConsumer isInsideProvider={false} />
      </div>
    );
  }
}

export default App;
