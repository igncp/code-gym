import React, { Component, PropTypes } from "react";

export class App extends Component {
  constructor(props) {
    super();

    console.log("props.start", props.start);

    this.state = { clicks: props.start };
  }

  handleButtonClick() {
    this.setState({ clicks: this.state.clicks + 1 });
  }

  render() {
    return (
      <div>
        <p>This is the app</p>
        <p>Click the button:</p>
        <p><button onClick={() => this.handleButtonClick()}>Click</button></p>
        <p>clicks: {this.state.clicks}</p>
      </div>
    );
  }
}

App.propTypes = { start: PropTypes.number.isRequired };
