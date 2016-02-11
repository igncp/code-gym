package main

import "fmt"

type person struct {
	name string
	age  int
}

type address struct {
	street   string
	number   int
	postcode string
}

type document struct {
	person  person
	address address
}

func main() {
	fmt.Println(person{"Bob", 20})
	fmt.Println(person{name: "Alice", age: 30})
	fmt.Println(person{name: "Fred"})
	fmt.Println(&person{name: "Ann", age: 40})
	s := person{name: "Sean", age: 50}
	fmt.Println(s.name)
	sp := &s
	fmt.Println(sp.age)
	sp.age = 51
	fmt.Println(sp.age)

	foo := person{"Foo", 50}
	bar := address{street: "bar st", number: 7, postcode: "28224"}
	doc1 := document{foo, bar}
	fmt.Println(doc1)
}
