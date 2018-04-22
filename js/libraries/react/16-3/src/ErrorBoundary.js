import React from 'react'

export class CompThatErrors extends React.Component {
  render() {
    // uncoment to test the error boundary (it will trigger the development
    // error overlay)
    // throw new Error('errror during render')

    return 'component without error'
  }
}

export class ErrorBoundary extends React.Component {
  constructor() {
    super()

    this.state = {}
  }

  componentWillMount() {
    this.setState({
      hasCatchedErr: false,
    })
  }

  componentDidCatch(err) {
    // console.log('there was a catched error: ', err)

    this.setState({
      hasCatchedErr: true
    })
  }

  render() {
    if (this.state.hasCatchedErr) {
      return (
        <div>
          {'The children comp threw an error'}
        </div>
      )
    }

    return (
      <div>
        {this.props.children}
      </div>
    )
  }
}
