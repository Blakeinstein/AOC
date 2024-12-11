package day_11

import (
	"fmt"
	"strconv"
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

type key struct {
	stone int
	count int
}

func changeStone(stone int, count int, memo map[key]int) int {
	if count == 0 {
		return 0
	}
	val, ok := memo[key{stone, count}]
	if ok {
		return val
	}

	total := 1
	value := stone
	for i := 0; i < count; i++ {
		if value == 0 {
			value = 1
			continue
		}

		numStr := fmt.Sprint(value)

		if (len(numStr) & 1) == 0 {
			stone1, _ := strconv.Atoi(numStr[:len(numStr)/2])
			stone2, _ := strconv.Atoi(numStr[len(numStr)/2:])

			total += changeStone(stone2, count-i-1, memo)
			value = stone1
			continue
		}

		value *= 2024
	}

	memo[key{stone, count}] = total

	return total
}

func parseInput(input string) []int {
	parts := strings.Split(input, " ")
	var list []int
	for _, part := range parts {
		num, err := strconv.Atoi(part)
		if err != nil {
			continue
		}
		list = append(list, num)
	}
	return list
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	length := 0
	head := parseInput(input[0])

	memo := make(map[key](int))

	for _, val := range head {
		length += changeStone(val, 26, memo)
	}

	return fmt.Sprintf("%v", length)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {

	length := 0
	head := parseInput(input[0])

	memo := make(map[key](int))

	for _, val := range head {
		length += changeStone(val, 76, memo)
	}

	return fmt.Sprintf("%v", length)
}
