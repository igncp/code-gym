package main

import (
	"fmt"
	"time"
)

func f(from string) {
	fmt.Println("Current time:", time.Now())
	for i := 0; i < 10000; i++ {
		fmt.Println(from, ":", i)
	}
}

func main() {
	f("direct")

	go f("goroutine")

	go func(msg string) {
		fmt.Println(msg)
	}("going")

	time.Sleep(1 * time.Nanosecond)
	fmt.Println("finishing script (some go routines might not have finished)")
}
