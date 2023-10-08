package main

import (
	"log"
	"os"
	"testing"
)

func TestDayExample(t *testing.T) {
	list := `
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
`

	halvesPriorities := GetHalvesPriorities(list)
	expected := 157
	if halvesPriorities != expected {
		t.Errorf("Expected %d, got %d", expected, halvesPriorities)
	}

	groupsPriorities := GetGroupsPriorities(list)
	expected = 70
	if groupsPriorities != expected {
		t.Errorf("Expected %d, got %d", expected, groupsPriorities)
	}
}

func TestInput(t *testing.T) {
	dat, err := os.ReadFile("./input.txt")
	if err != nil {
		log.Fatal("Error reading file: ", err)
	}

	file_content := string(dat)

	halvesPriorities := GetHalvesPriorities(file_content)
	expected := 8085
	if halvesPriorities != expected {
		t.Errorf("Expected %d, got %d", expected, halvesPriorities)
	}

	groupsPriorities := GetGroupsPriorities(file_content)
	expected = 2515
	if groupsPriorities != expected {
		t.Errorf("Expected %d, got %d", expected, groupsPriorities)
	}
}
