import {Logger} from "./Logger"

type greetingsCollection = Array <string>

const greetingsExamples: greetingsCollection = ["Hello", "Hiya"]

function getRandomGreeting(greetings: greetingsCollection): string {
  const factor: number = Math.random()
  return greetings[Math.floor(factor * greetings.length)]
}

export default class Greeter {
  private suffix: string = "!"
  constructor(private subject: string, private logger: Logger) { }
  greet(alternativeSuffix?: string) {
    const verb: string = getRandomGreeting(greetingsExamples)
    this.logger.log(`${this.subject}${alternativeSuffix || this.suffix}`)
  }
}
