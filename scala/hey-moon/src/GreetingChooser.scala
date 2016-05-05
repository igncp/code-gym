package app

import java.util.Random

object greetingChooser {
  val rand = new Random(System.currentTimeMillis())
  def getRandom(): String = {
    val greetings = Array("Hello", "Hey", "Aloha")
    val randomIndex = rand.nextInt(greetings.length)

    return greetings(randomIndex) 
  }
}