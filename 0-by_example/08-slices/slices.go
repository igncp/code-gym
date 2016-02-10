// Arrays vs Slices: http://blog.golang.org/go-slices-usage-and-internals

package main

import "fmt"

func print_slice(slice_name string, slice []int) {
	fmt.Printf("Slice %v: s -> s:%v, cap(s):%v, len(s):%v\n", slice_name, slice, cap(slice), len(slice))
}

func main() {
	s := make([]int, 3)
	fmt.Println("emp:", s)
	s[0] = 0
	s[1] = 1
	s[2] = 2
	print_slice("s", s)
	fmt.Println("get:", s[2])
	fmt.Println("len:", len(s))
	s = append(s, 3)
	s = append(s, 4, 5)
	fmt.Println("apd:", s)
	c := make([]int, len(s))
	copy(c, s)
	fmt.Println("cpy:", c)
	l := s[2:5]
	fmt.Println("sl1:", l)
	l = s[:5]
	fmt.Println("sl2:", l)
	l = s[2:]
	fmt.Println("sl3:", l)
	t := []string{"g", "h", "i"}
	fmt.Println("dcl:", t)
	twoD := make([][]int, 3)
	for i := 0; i < 3; i++ {
		innerLen := i + 1
		twoD[i] = make([]int, innerLen)
		for j := 0; j < innerLen; j++ {
			twoD[i][j] = i + j
		}
	}
	fmt.Println("2d: ", twoD)
}
