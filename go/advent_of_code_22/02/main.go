package main

import (
	"errors"
	"strings"
)

const (
	FirstApproach  = iota
	SecondApproach = iota
)

type Choice int

const (
	Rock Choice = iota
	Paper
	Scissors
)

type Result int

const (
	You Result = iota
	Other
	Draw
)

type Round struct {
	You   Choice
	Other Choice
}

func parseListIntoRounds(list string, approach int) ([]Round, error) {
	lines := strings.Split(list, "\n")
	rounds := make([]Round, 0)

	getYou := func(fragment string, _ Choice) (Choice, error) {
		switch fragment {
		case "X":
			return Rock, nil
		case "Y":
			return Paper, nil
		case "Z":
			return Scissors, nil
		}

		return -1, errors.New("Unknown case")
	}

	if approach != FirstApproach {
		getYou = func(fragment string, other Choice) (Choice, error) {
			switch fragment {
			case "X":
				return getLoserPlay(other)
			case "Y":
				return other, nil
			case "Z":
				return getWinnerPlay(other)
			}
			return -1, errors.New("Unknown case")
		}
	}

	for _, line := range lines {
		if len(line) == 0 {
			continue
		}

		fragments := strings.Split(line, " ")

		other := Rock

		switch fragments[0] {
		case "A":
			other = Rock
		case "B":
			other = Paper
		case "C":
			other = Scissors
		}

		you, err := getYou(fragments[1], other)

		if err != nil {
			return nil, err
		}

		rounds = append(rounds, Round{
			You:   you,
			Other: other,
		})
	}

	return rounds, nil
}

func getWinnerPlay(play Choice) (Choice, error) {
	switch play {
	case Rock:
		return Paper, nil
	case Paper:
		return Scissors, nil
	case Scissors:
		return Rock, nil
	}

	return -1, errors.New("Unknown play")
}

func getLoserPlay(play Choice) (Choice, error) {
	switch play {
	case Rock:
		return Scissors, nil
	case Paper:
		return Rock, nil
	case Scissors:
		return Paper, nil
	}

	return -1, errors.New("Unknown play")
}

func getRoundWinner(round Round) (Result, error) {
	if round.You == round.Other {
		return Draw, nil
	}

	winnerPlay, err := getWinnerPlay(round.Other)

	if err != nil {
		return -1, err
	}

	if round.You == winnerPlay {
		return You, nil
	}

	return Other, nil
}

func getShapeScore(round Round) (int, error) {
	switch round.You {
	case Rock:
		return 1, nil
	case Paper:
		return 2, nil
	case Scissors:
		return 3, nil
	}

	return -1, errors.New("Unknown shape")
}

func getWinnerScore(winner Result) (int, error) {
	switch winner {
	case You:
		return 6, nil
	case Other:
		return 0, nil
	case Draw:
		return 3, nil
	}

	return -1, errors.New("Unknown winner")
}

func getRoundScore(round Round) (int, error) {
	winner, err := getRoundWinner(round)
	if err != nil {
		return -1, err
	}

	winnerScore, err := getWinnerScore(winner)
	if err != nil {
		return -1, err
	}

	shapeScore, err := getShapeScore(round)
	if err != nil {
		return -1, err
	}

	return winnerScore + shapeScore, nil
}

func GetRoundsScore(list string, approach int) (int, error) {
	rounds, err := parseListIntoRounds(list, approach)
	if err != nil {
		return -1, err
	}

	score := 0
	for _, round := range rounds {
		newScore, err := getRoundScore(round)
		if err != nil {
			return -1, err
		}
		score += newScore
	}

	return score, nil
}
