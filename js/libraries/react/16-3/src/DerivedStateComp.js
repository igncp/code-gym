import React, { Component } from 'react'

class DerivedStateComp extends Component {
  static getDerivedStateFromProps(nextProps, prevState) {
    return {
      timesPropsChanged: prevState.timesPropsChanged + 1
    }
  }

  constructor() {
    super()

    this.state = {
      counter: 0,
      timesPropsChanged: 0,
    }
  }

  UNSAFE_componentWillMount() {
    // this will not be called
    console.log('UNSAFE_componentWillMount not called')
  }

  render() {
    return (
      <div>
        <p>{'DerivedStateComp - Counter: ' + this.state.counter}</p>
        <p>{'TimesPropsChanged: ' + this.state.timesPropsChanged}</p>
        <button
          onClick={() => this.setState(state => ({ counter: state.counter + 1 }))}
        >
          {'Increase the counter'}
        </button>
      </div>
    )
  }
}

export default DerivedStateComp
