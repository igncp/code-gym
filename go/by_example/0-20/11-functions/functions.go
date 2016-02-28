package main

import (
	"fmt"
	"hash"
	"hash/adler32"
)

func plus(a int, b int) int {
	return a + b
}

func plusPlus(a, b, c int) int {
	return a + b + c
}

type Hash32 hash.Hash32

func hashSliceFactory(length int) []Hash32 {
	fmt.Println(length)
	hashSlice := []Hash32{}
	for j := 0; j <= 10; j++ {
		hashSlice = append(hashSlice, adler32.New())
	}
	return hashSlice
}

func main() {
	res := plus(1, 2)
	fmt.Println("1+2 =", res)
	res = plusPlus(1, 2, 3)
	fmt.Println("1+2+3 =", res)
	hashArray := hashSliceFactory(2)
	fmt.Println("hash array", hashArray)
	fmt.Println("hash [0] size", hashArray[0].Size())
}
