import app.greetingChooser
import utils.logger

object HelloMoon{
  def main(args: Array[String]): Unit = {
    var greeting = greetingChooser.getRandom()
    logger.log(greeting + ", moon")
  }
}