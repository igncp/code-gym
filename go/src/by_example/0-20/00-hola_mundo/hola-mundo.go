package main

import "fmt" // Package fmt implements formatted I/O with functions analogous to C's printf and scanf. The format 'verbs' are derived from C's but are simpler.

func main() {
	fmt.Println("Hola Mundo!")
	fmt.Println("What is your name?")

	var name string
	n, err := fmt.Scanln(&name)

	if err != nil {
		fmt.Println(n, err)
	}

	fmt.Printf("Hello %s!", name)
}
