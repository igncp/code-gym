package main

import "fmt"

func main() {
	nums := []int{1, 10, 100}
	sum := 0
	for _, num := range nums {
		sum += num
	}
	fmt.Println("sum: 111 ===", sum)
	for i, num := range nums {
		if num == 100 {
			fmt.Println("index: 2 ===", i)
		}
	}
	kvs := map[string]string{"a": "apple", "b": "banana"}
	for k, v := range kvs {
		fmt.Printf("%s -> %s\n", k, v)
	}
	for i, c := range "go" {
		fmt.Println(i, c)
	}
}
