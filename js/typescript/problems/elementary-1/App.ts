import ConsoleLogger from "./ConsoleLogger"
import Greeter from "./Greeter"

export default class App {
  bootstrap(): void {
    const logger = new ConsoleLogger()
    const greeter = new Greeter("Hello World", logger)
    greeter.greet(" :)")
  }
}
