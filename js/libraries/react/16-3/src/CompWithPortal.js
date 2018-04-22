import React, { Component } from 'react'
import ReactDOM from 'react-dom'

const EL_ID = 'fooId'

class CompWithPortal extends Component {
  componentWillMount() {
    const origEl = this.el || document.getElementById(EL_ID)

    if (origEl) {
      this.el = origEl

      return;
    }

    const el = document.createElement('div')

    el.setAttribute('id', EL_ID)

    document.body.prepend(el)

    this.el = el
  }

  render() {
    return ReactDOM.createPortal(
      (
        <div style={{ color: 'white', backgroundColor: 'black' }}>
          {'This is the content of comp with portal'}
        </div>
      ),
      this.el
    )
  }
}

export default CompWithPortal
