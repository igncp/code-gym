package main

import renamed_fmt "fmt"

func main() {
	if 7%2 == 0 {
		renamed_fmt.Println("7 is even")
	} else {
		renamed_fmt.Println("7 is odd")
	}
	if 8%4 == 0 {
		renamed_fmt.Println("8 is divisible by 4")
	}
	if num := 9; num < 0 {
		renamed_fmt.Println(num, "is negative")
	} else if num < 10 {
		renamed_fmt.Println(num, "has 1 digit")
	} else {
		renamed_fmt.Println(num, "has multiple digits")
	}

	c := map[bool]int{true: 1, false: 0}[5 > 4]
	renamed_fmt.Println("c", c)
}
