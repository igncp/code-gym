package main

import (
	"bytes"
	"fmt"
	"text/template"
)

func sum(nums ...int) {
	fmt.Print(nums, " ")
	total := 0
	for _, num := range nums {
		total += num
	}
	fmt.Println(total)
}

func runThroughTemplate(parts ...string) string {
	out := new(bytes.Buffer)
	t, _ := template.New("template1").Parse(`{{define "T"}}Hello, {{.}}!{{end}}`)

	t.ExecuteTemplate(out, "T", "variadic functions")

	return out.String()
}

func main() {
	sum(1, 2)
	sum(1, 2, 3)
	nums := []int{1, 2, 3, 4}
	sum(nums...)
	parts := []string{"variadic", "functions"}
	result := runThroughTemplate(parts...)
	fmt.Println("result:", result)
}
