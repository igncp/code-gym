package main

import "strings"

func getHalvesPriority(priorityList map[string]int, line string) int {
	numOfChars := len(line)
	firstHalf := line[:numOfChars/2]
	secondHalf := line[numOfChars/2:]
	firstHalfMap := make(map[rune]bool)

	for _, char := range firstHalf {
		firstHalfMap[char] = true
	}

	for _, char := range secondHalf {
		if firstHalfMap[char] {
			return priorityList[string(char)]
		}
	}

	return 0
}

func getGroupsPriority(priorityList map[string]int, lineA string, lineB string, lineC string) int {
	firstLineMap := make(map[rune]bool)
	secondLineMap := make(map[rune]bool)

	for _, char := range lineA {
		firstLineMap[char] = true
	}

	for _, char := range lineB {
		secondLineMap[char] = true
	}

	for _, char := range lineC {
		if firstLineMap[char] && secondLineMap[char] {
			return priorityList[string(char)]
		}
	}

	return 0
}

func getPrioritiesList(list string) map[string]int {
	priorityList := make(map[string]int)
	letters := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

	index := 1
	for _, letter := range letters {
		priorityList[string(letter)] = index
		index++
	}

	return priorityList
}

func GetHalvesPriorities(list string) int {
	halvesPriorities := 0
	lines := strings.Split(list, "\n")
	priorityList := getPrioritiesList(list)

	for _, line := range lines {
		halvesPriorities += getHalvesPriority(priorityList, line)
	}

	return halvesPriorities
}

func GetGroupsPriorities(list string) int {
	groupsPriorities := 0
	lines := strings.Split(list, "\n")
	priorityList := getPrioritiesList(list)
	filteredLines := []string{}

	for _, line := range lines {
		if len(line) > 0 {
			filteredLines = append(filteredLines, line)
		}
	}

	for i := 0; i < len(filteredLines)-2; i += 3 {
		groupsPriorities += getGroupsPriority(priorityList, filteredLines[i], filteredLines[i+1], filteredLines[i+2])
	}

	return groupsPriorities
}
