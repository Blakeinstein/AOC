package day_02

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/blakeinstein/AOC/2024/internal/utils"
)

// isSafe checks if all numbers in the list are in increasing or decreasing order
// and the absolute difference between subsequent numbers is between 1 and 3
func isSafe(numbers []int) bool {
	motion := numbers[1] > numbers[0]

	for i := 1; i < len(numbers); i++ {
		diff := numbers[i] - numbers[i-1]
		if !(motion == (diff > 0) &&
			diff != 0 &&
			utils.Abs(diff) <= 3) {
			return false
		}
	}

	return true
}

// parseInput parses the input string into a list of list of ints
func parseInput(input []string) [][]int {
	result := make([][]int, len(input))

	for i, line := range input {
		if line == "" {
			continue
		}
		strNumbers := strings.Split(line, " ")
		intNumbers := make([]int, len(strNumbers))
		for j, strNum := range strNumbers {
			num, err := strconv.Atoi(strNum)
			if err != nil {
				panic(err)
			}
			intNumbers[j] = num
		}
		result[i] = intNumbers
	}

	return result
}

// Run function of the daily challenge
func Run(input []string, mode int) {
	if mode == 1 || mode == 3 {
		fmt.Printf("Part one: %v\n", Part1(input))
	}
	if mode == 2 || mode == 3 {
		fmt.Printf("Part two: %v\n", Part2(input))
	}
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	data := parseInput(input)

	count := 0

	for _, numbers := range data {
		if isSafe(numbers) {
			count++
		}
	}

	return fmt.Sprintf("%d", count)
}

func isAlmostSafe(numbers []int) bool {
	for i := range numbers {
		newList := make([]int, len(numbers)-1)
		copy(newList, numbers[:i])
		copy(newList[i:], numbers[i+1:])
		if isSafe(newList) {
			return true
		}
	}
	return false

}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	data := parseInput(input)

	count := 0

	for _, numbers := range data {
		if isSafe(numbers) || isAlmostSafe(numbers) {
			count++
		}
	}

	return fmt.Sprintf("%d", count)
}
