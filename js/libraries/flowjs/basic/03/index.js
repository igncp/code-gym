// @flow

interface IPlace {}

interface ITransportation {
  goTo(place: IPlace): boolean
}

type Brand = "Foo" | "Bar"

type Engine = {|
  brand: Brand
|}

class Car implements ITransportation {
  engine: Engine
  location: IPlace
  constructor({engine}: {engine: Engine}) {
    this.engine = engine
  }
  goTo(place: IPlace): boolean {
    this.location = place

    return true
  }
}

const engine: Engine = {
  brand: "Foo",
}

const fordFiesta: Car = new Car({engine})

console.log("fordFiesta", fordFiesta)
