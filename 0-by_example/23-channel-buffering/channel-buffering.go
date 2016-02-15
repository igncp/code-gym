package main

import "fmt"

func main() {
	messages := make(chan string, 2)
	arraysChannel := make(chan [2]string, 2)

	messages <- "buffered"
	messages <- "channel"

	fmt.Println(<-messages)
	fmt.Println(<-messages)

	fmt.Println("* Buffered channels also support more complex data:")

	arraysChannel <- [2]string{"foo", "bar"}
	arraysChannel <- [2]string{"baz", "!"}

	fmt.Println(<-arraysChannel)
	fmt.Println(<-arraysChannel)
}
