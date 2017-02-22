/// <reference path="_declare/chalk.d.ts" />
import chalk = require("chalk")
import {Logger} from "./Logger"

export default class ConsoleLogger implements Logger {
  log(message: string) {
    console.log(chalk.blue(message))
  }
}
