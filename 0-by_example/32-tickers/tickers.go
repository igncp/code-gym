package main

import (
	"fmt"
	"time"
)

func runTicker() *time.Ticker {
	ticker := time.NewTicker(time.Millisecond * 500)

	go func() {
		for t := range ticker.C {
			fmt.Println("Tick at", t)
		}
	}()

	return ticker
}

func main() {
	ticker := runTicker()
	timer := time.NewTimer(3 * time.Second)

	select {
	case <-timer.C:
		ticker.Stop()
	case <-time.After(5 * time.Second):
		ticker.Stop()
	}

	fmt.Println("Ticker stopped")
}
