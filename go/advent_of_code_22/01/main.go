package main

import (
	"errors"
	"log"
	"slices"
	"sort"
	"strconv"
	"strings"
)

type CaloriesResult struct {
	top_one   int
	top_three int
}

func GetCalories(calories_list string) (*CaloriesResult, error) {
	lines := strings.Split(calories_list, "\n")

	elf_index := 0
	elf_calories := []int{0}

	for _, line := range lines {
		if len(line) == 0 {
			elf_index++
			elf_calories = append(elf_calories, 0)
			continue
		}

		line_calories, err := strconv.Atoi(line)
		if err != nil {
			log.Fatal("Error converting line to int: ", err)
		}

		elf_calories[elf_index] += line_calories
	}

	if len(elf_calories) < 3 {
		return &CaloriesResult{}, errors.New("Not enough elves")
	}

	sort.Ints(elf_calories)
	slices.Reverse(elf_calories)

	result := CaloriesResult{
		top_one:   elf_calories[0],
		top_three: elf_calories[0] + elf_calories[1] + elf_calories[2],
	}

	return &result, nil
}
