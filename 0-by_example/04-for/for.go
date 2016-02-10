package main

import "math/rand"

import "fmt"

func main() {
	i := 1
	for i <= 3 {
		fmt.Println(i)
		i += 1
	}

	for j := 7; j <= 9; j++ {
		fmt.Println(j)
	}

	for j := 1; j <= 3; j++ {
		fmt.Println(rand.Intn(j))
	}

	for {
		fmt.Println("loop")
		break
	}

	// http://golangtutorials.blogspot.co.uk/2011/06/control-structures-go-for-loop-break.html
	s := ""
	for s != "aaaaa" {
		fmt.Println("Value of s:", s)
		s = s + "a"
	}
}
