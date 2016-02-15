package main

import (
	"github.com/fatih/color"
)

func ping(pings chan<- string, msg string) {
	pings <- msg
}

func pong(pings <-chan string, pongs chan<- string) {
	msg := <-pings
	pongs <- msg
}

func main() {
	co := color.New(color.FgCyan).Add(color.Underline).PrintlnFunc()

	pings := make(chan string, 1)
	pongs := make(chan string, 1)

	ping(pings, "passed message")
	pong(pings, pongs)

	co(<-pongs)
}
