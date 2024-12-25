package day_01

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/blakeinstein/AOC/2024/internal/utils"
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

func parseInput(input []string) ([]int, []int) {
	list1 := []int{}
	list2 := []int{}
	for _, line := range input {
		parts := strings.Split(line, "   ")
		if len(parts) == 2 {
			part1, err1 := strconv.Atoi(parts[0])
			part2, err2 := strconv.Atoi(parts[1])
			if err1 == nil && err2 == nil {
				list1 = append(list1, part1)
				list2 = append(list2, part2)
			}
		}
	}
	return list1, list2
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	list1, list2 := parseInput(input)
	sort.Ints(list1)
	sort.Ints(list2)
	sum := 0
	for i := range list1 {
		sum += utils.Abs(list1[i] - list2[i])
	}
	return fmt.Sprintf("%d", sum)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	list1, list2 := parseInput(input)
	frequency := make(map[int]int)
	for _, num := range list2 {
		frequency[num]++
	}

	sum := 0
	for _, num := range list1 {
		sum += num * frequency[num]
	}

	return fmt.Sprintf("%d", sum)
}
