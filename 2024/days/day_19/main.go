package day_19

import (
	"fmt"
	"strings"
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

func parseInput(input []string) (towels []string, goals []string) {
	towels = strings.Split(input[0], ", ")
	goals = input[2:]

	return
}

func design(goal string, towels []string, cache map[string]int) int {
	res := 0
	for _, towel := range towels {
		if num, ok := cache[goal]; ok {
			return num
		}
		if goal == "" {
			return 1
		}
		if strings.HasPrefix(goal, towel) {
			res += design(goal[len(towel):], towels, cache)
		}
	}
	cache[goal] = res
	return res

}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	towels, goals := parseInput(input)

	count := 0
	for _, goal := range goals {
		cache := map[string]int{}
		if design(goal, towels, cache) > 0 {
			count++
		}
	}
	return fmt.Sprintf("%d", count)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	towels, goals := parseInput(input)

	count := 0
	for _, goal := range goals {
		cache := map[string]int{}
		count += design(goal, towels, cache)
	}
	return fmt.Sprintf("%d", count)
}
