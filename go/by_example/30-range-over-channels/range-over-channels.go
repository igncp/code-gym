package main

import "fmt"

func main() {
	queueA := make(chan string, 2)
	queueA <- "one"
	queueA <- "two"
	close(queueA)

	for elem := range queueA {
		fmt.Println(elem)
	}

	queueB := make(chan string, 2)
	queueB <- "three"
	queueB <- "four"
	close(queueB)

	for elem := range queueB {
		fmt.Println(elem)
		break
	}
}
