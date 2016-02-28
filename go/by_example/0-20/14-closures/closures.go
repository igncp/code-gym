package main

import (
	"fmt"
	"path/filepath"
)

func intSeq() func() int {
	i := 0
	return func() int {
		i += 1
		return i
	}
}

func getPathFragmentsGetter(s string) func() string {
	fragments := filepath.SplitList(s)
	position := -1

	return func() string {
		position += 1
		return fragments[position]
	}
}

func main() {
	nextInt := intSeq()
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	newInts := intSeq()
	fmt.Println(newInts())
	pathString := "foo/bar:baz/bam"

	nextFragment := getPathFragmentsGetter(pathString)
	fmt.Println(pathString)

	fmt.Println(nextFragment())
	fmt.Println(nextFragment())
}
