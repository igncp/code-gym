package main

import (
	"log"
	"os"
	"testing"
)

func TestDayExample(t *testing.T) {
	list := `
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
`
	result, err := GetCalories(list)

	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}

	if result.top_one != 24000 {
		t.Errorf("Expected 24000, got %d", result.top_one)
	}

	if result.top_three != 45000 {
		t.Errorf("Expected 45000, got %d", result.top_three)
	}
}

func TestInput(t *testing.T) {
	dat, err := os.ReadFile("./input.txt")
	if err != nil {
		log.Fatal("Error reading file: ", err)
	}

	file_content := string(dat)

	result, err := GetCalories(file_content)

	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}

	if result.top_one != 66487 {
		t.Errorf("Expected 66487, got %d", result.top_one)
	}

	if result.top_three != 197301 {
		t.Errorf("Expected 197301, got %d", result.top_three)
	}
}

func TestError(t *testing.T) {
	list := `1000
2000
3000

4000`

	_, err := GetCalories(list)

	if err == nil {
		t.Errorf("Expected error, got nil")
	}
}
