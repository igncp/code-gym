// @flow

const message: string = "Hello world"

const log: (...data: any[]) => void = (...data: any[]) => {
  console.log(...data)
}

log(message)
