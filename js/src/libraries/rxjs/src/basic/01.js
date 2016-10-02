const chai = require("chai")
const {spy} = require("sinon")
const Rx = require("rxjs")
const {expect} = chai

chai.use(require("sinon-chai"))

const s = spy()

describe("basic 01", function() {

  beforeEach(() => {

    s.reset()

  })

  it("Observable.of", () => {

    const obv = Rx.Observable.of("foo", "bar")

    obv.subscribe(s)
    expect(s).to.have.been.calledTwice

  })

  it("Observable.from", () => {

    const obv = Rx.Observable.from(["foo", "bar", "baz"])

    obv.subscribe(s)
    expect(s).to.have.been.calledThrice

  })

})

