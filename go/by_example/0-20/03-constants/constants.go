package main

import "fmt"
import "math"

// http://www.dotnetperls.com/const-go
const (
	Cat  = 10
	Dog  = 20
	Bird = 30
)

// Iota. This is an enumerator for const creation. The Go compiler starts iota at 0 and increments it by one for each following constant. We can use it in expressions.
const (
	Low = 1 * iota
	Medium
	High
)

func main() {
	const s string = "constant"
	fmt.Println("s", s)
	const n = 500000000
	const d = 3e20 / n
	fmt.Println("d", d)
	fmt.Println("int64(d)", int64(d))
	fmt.Println("math.Sin(n)", math.Sin(n))
	fmt.Println("Dog", Dog)
	fmt.Println("Medium", Medium)
}
