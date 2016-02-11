package main

import (
	"fmt"
	"math"
	"math/cmplx"
)

func zeroval(ival int) {
	ival = 0
}

func zeroptr(iptr *int) {
	*iptr = 0
}

func add180ToComplexNumner(complexNumber *complex128) {
	r, θ := cmplx.Polar(*complexNumber)
	*complexNumber = cmplx.Rect(r, θ+math.Pi/2)
}

func main() {
	i := 1

	fmt.Println("initial:", i)
	zeroval(i)

	fmt.Println("zeroval:", i)
	zeroptr(&i)

	fmt.Println("zeroptr:", i)
	fmt.Println("pointer:", &i)

	complex1 := cmplx.Rect(12, math.Pi/2)
	fmt.Println(complex1)
	add180ToComplexNumner(&complex1)
	fmt.Println(complex1)
}
