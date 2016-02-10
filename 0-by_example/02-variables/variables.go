package main

import "fmt"

func p(title string, value interface{}) {
	fmt.Println(title, value)
}

func main() {
	var a string = "initial"
	p("a:", a)

	var b, c int = 1, 2
	p("b:", b)
	p("c:", c)

	var d = true
	p("d:", d)

	var e int
	p("e:", e)

	f := "short"
	p("f:", f)

	var anything interface{} = "I'm a string" // An interface{} type is a type that could be any value. It’s like Object in Java.
	p("anything:", anything)

	var foo interface{}
	switch foo.(type) {
	case string:
		fmt.Println("foo is a string")
	case int32, int64:
		fmt.Println("foo is an int")
	default:
		fmt.Println("foo is other than a string or an int")
	}

}
