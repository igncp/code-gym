import React from 'react'

const COLORS = ['blue', 'red', 'green', 'orange']

const {Consumer, Provider} = React.createContext({
  updateBackground: () => null,
  backgroundColor: COLORS[0],
})

export class CompContextConsumer extends React.Component {
  updateBackground(backgroundColor, updateBackground) {
    const newColorsArr = COLORS.filter(c => c !== backgroundColor)
    const randomColorIdx = Math.floor(Math.random() * newColorsArr.length)
    const randomColor = newColorsArr[randomColorIdx]

    updateBackground(randomColor)
  }

  render() {
    return (
      <Consumer>{({backgroundColor, updateBackground}) => (
        <div style={{ backgroundColor }}>
          <p>{'This component is a context consumer and it is ' +
            (this.props.isInsideProvider ? 'inside Provider' : 'outside Provider')}</p>
          <p><button onClick={() => this.updateBackground(backgroundColor, updateBackground)}>{'Update'}</button></p>
        </div>
      )}</Consumer>
    )
  }
}

export class CompContextProvider extends React.Component {
  state = {
    backgroundColor: COLORS[1]
  }

  render() {
    return (
      <div>
        {'This component is a context provider'}
        <Provider
          value={{
            backgroundColor: this.state.backgroundColor,
            updateBackground: (color) => this.setState({backgroundColor: color}),
          }}
        >
          {this.props.children}
        </Provider>
      </div>
    )
  }
}
