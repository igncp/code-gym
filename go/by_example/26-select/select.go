package main

import (
	"fmt"
	"github.com/fatih/color"
	"gopkg.in/kyokomi/emoji.v1"
	"time"
)

func main() {
	cy := color.New(color.FgCyan).Add(color.BlinkSlow).PrintfFunc()
	ma := color.New(color.FgMagenta).Add(color.BlinkSlow).PrintfFunc()

	c1 := make(chan string)
	c2 := make(chan string)

	go func() {
		time.Sleep(time.Second * 1)
		c1 <- "one"
	}()

	go func() {
		time.Sleep(time.Second * 2)
		c2 <- "two"
	}()

	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-c1:
			cy("received %v ", msg1)
			emoji.Printf(":beer:\n")
		case msg2 := <-c2:
			ma("received %v ", msg2)
			emoji.Printf(":pizza:")
		}
	}
	fmt.Println("")
}
