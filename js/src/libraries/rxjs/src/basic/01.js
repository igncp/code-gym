const chai = require("chai")
const {spy} = require("sinon")
const Rx = require("rxjs")
const {expect} = chai

chai.use(require("sinon-chai"))

describe("basic 01", function() {

  it("Observable.of with arguments", () => {

    const obv = Rx.Observable.of("foo", "bar")
    const s = spy()

    obv.subscribe(s)
    expect(s).to.have.been.calledTwice

  })

})

