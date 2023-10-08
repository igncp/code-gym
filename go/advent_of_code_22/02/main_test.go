package main

import (
	"log"
	"os"
	"testing"
)

func TestDayExample(t *testing.T) {
	list := `
A Y
B X
C Z
`

	score, err := GetRoundsScore(list, FirstApproach)
	expected := 15
	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}
	if score != expected {
		t.Errorf("Expected score of %d, got %d", expected, score)
	}

	score, err = GetRoundsScore(list, SecondApproach)
	expected = 12
	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}
	if score != expected {
		t.Errorf("Expected score of %d, got %d", expected, score)
	}
}

func TestListError(t *testing.T) {
	list := `
A H
B X
C Z
`

	_, err := GetRoundsScore(list, FirstApproach)
	if err == nil {
		t.Errorf("Expected error, got nil")
	}

	_, err = GetRoundsScore(list, SecondApproach)
	if err == nil {
		t.Errorf("Expected error, got nil")
	}
}

func TestDayInputFirstApproach(t *testing.T) {
	dat, err := os.ReadFile("./input.txt")
	if err != nil {
		log.Fatal("Error reading file: ", err)
	}

	file_content := string(dat)

	score, err := GetRoundsScore(file_content, FirstApproach)
	expected := 13446
	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}
	if score != expected {
		t.Errorf("Expected score of %d, got %d", expected, score)
	}

	score, err = GetRoundsScore(file_content, SecondApproach)
	expected = 13509
	if err != nil {
		t.Errorf("Expected no error, got %s", err)
	}
	if score != expected {
		t.Errorf("Expected score of %d, got %d", expected, score)
	}
}
