package main

import (
	"container/ring"
	"fmt"
	"os"
)

func vals() (int, int) {
	return 3, 7
}

func current_extremes(r *ring.Ring) (int, int) {
	length := r.Len()
	if length%2 != 0 {
		fmt.Fprintln(os.Stderr, "error: Ring must be even")
		os.Exit(1)
	}
	fmt.Println(r.Value)

	return r.Value.(int), r.Move(length / 2).Value.(int)
}

func main() {
	a, b := vals()

	fmt.Println(a)
	fmt.Println(b)

	_, c := vals()
	fmt.Println(c)

	ring1 := ring.New(10)
	for j := 0; j <= 10; j++ {
		ring1 = ring1.Next()
		ring1.Value = j
	}

	d, e := current_extremes(ring1)
	fmt.Println("current extreme 1:", d)
	fmt.Println("current extreme 2:", e)
}
