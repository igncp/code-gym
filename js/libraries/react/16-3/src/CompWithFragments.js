import React from 'react'

const fragmentsEl = (
  <React.Fragment>
    <div>{'One Item'}</div>
    <div>{'Other Item'}</div>
  </React.Fragment>
)

const CompWithFragments = () => {
  return (
    <div
      style={{
        border: 'solid 1px black'
      }}
    >
      {'This is a component with fragments'}
      {fragmentsEl}
    </div>
  );
}

export default CompWithFragments
