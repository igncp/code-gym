package main

import (
	"fmt"
	"github.com/hoisie/mustache"
	"strconv"
	"time"
)

func printWithMoustache(template string, context map[string]string) {
	message := mustache.Render(template, context)
	fmt.Println(message)
}

func printTimeout(i int) {
	printWithMoustache("timeout: {{number}}.", map[string]string{"number": strconv.Itoa(i)})
}

func printResult(letter string) {
	printWithMoustache("Result! {{letter}}", map[string]string{"letter": letter})
}

func main() {
	c1 := make(chan string, 1)

	go func() {
		time.Sleep(time.Second * 2)
		c1 <- "A"
	}()

	select {
	case res := <-c1:
		printResult(res)
	case <-time.After(time.Second * 1):
		printTimeout(1)
	}

	c2 := make(chan string, 1)
	go func() {
		time.Sleep(time.Second * 2)
		c2 <- "B"
	}()

	select {
	case res := <-c2:
		printResult(res)
	case <-time.After(time.Second * 3):
		printTimeout(2)
	}
}
