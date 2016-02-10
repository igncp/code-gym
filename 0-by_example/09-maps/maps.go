package main

import "fmt"

func get_keys(mp map[string]int) []string {
	keys := make([]string, 0, len(mp))
	for k := range mp {
		keys = append(keys, k)
	}
	return keys
}

func main() {
	m := make(map[string]int)
	m["k1"] = 7
	m["k2"] = 13
	fmt.Println("map:", m)
	v1 := m["k1"]
	fmt.Println("v1: ", v1)
	fmt.Println("len:", len(m))
	delete(m, "k2")
	fmt.Println("map:", m)
	_, prs := m["k2"]
	fmt.Println("prs:", prs)
	n := map[string]int{"foo": 1, "bar": 2}
	fmt.Println("map:", n)
	fmt.Println("map keys:", get_keys(n))

}
