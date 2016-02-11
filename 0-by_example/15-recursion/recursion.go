package main

import (
	"fmt"
	"time"
)

func fact(n int) int {
	if n == 0 {
		return 1
	}
	return n * fact(n-1)
}

func printWithTime(name string, fn func() int) {
	start := time.Now()
	fmt.Println(name, fn())
	elapsed := time.Since(start)
	fmt.Printf("It took %s", elapsed)
	fmt.Println("")
}

func main() {
	printWithTime("fact(7)", func() int { return fact(7) })
	printWithTime("fact(15)", func() int { return fact(15) })
}
