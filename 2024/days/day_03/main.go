package day_03

import (
	"fmt"
	"regexp"
	"strconv"
)

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
	re := regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	var sum = 0
	for _, line := range input {
		matches := re.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			n1, err1 := strconv.Atoi(match[1])
			n2, err2 := strconv.Atoi(match[2])
			if err1 == nil && err2 == nil {
				sum += n1 * n2
			}
		}
	}
	return fmt.Sprintf("%d", sum)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	var sum = 0

	re := regexp.MustCompile(`(do)(n't)?\(\)|(mul)\((\d+),(\d+)\)`)

	flag := true

	for _, line := range input {
		matches := re.FindAllStringSubmatch(line, -1)

		for _, match := range matches {
			if match[1] == "do" {
				flag = len(match[2]) == 0
				continue
			}
			if !flag {
				continue
			}

			n1, err1 := strconv.Atoi(match[4])
			n2, err2 := strconv.Atoi(match[5])

			if err1 == nil && err2 == nil {
				sum += n1 * n2
			}
		}
	}

	return fmt.Sprintf("%d", sum)
}
