#!/bin/sh
exec scala "$0" "$@"
!#
object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hola mundo!")
  }
}
HelloWorld.main(args)