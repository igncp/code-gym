package main

import (
	"fmt"
	"math/rand"
	"time"
)

func pingRandomly(i int, c chan int) {
	interval := rand.Intn(500)
	fmt.Printf("The index %v will last %v milliseconds in send to channel\n", i, interval)
	time.Sleep(time.Duration(interval) * time.Millisecond)
	c <- i
}

func main() {
	messages := make(chan int, 4)
	rand.Seed(time.Now().UTC().UnixNano())

	go pingRandomly(0, messages)
	go pingRandomly(1, messages)
	go pingRandomly(2, messages)
	go pingRandomly(3, messages)

	for i := 0; i < 4; i++ {
		fmt.Printf("The index %v arrived\n", <-messages)
	}
}
