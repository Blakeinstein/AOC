package day_10

import (
	"fmt"
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

func parseInput(input []string) [][]int {
	result := [][]int{}
	for _, line := range input {
		row := []int{}
		for _, char := range line {
			row = append(row, int(char-'0'))
		}
		result = append(result, row)
	}
	return result
}

func calculateScore(topology [][]int, i, j, elevation int, visited map[[2]int]bool) int {
	if elevation == 9 {
		pos := [2]int{i, j}
		if !visited[pos] {
			visited[pos] = true
			return 1
		}
		return 0
	}

	score := 0

	for x := -1; x <= 1; x++ {
		for y := -1; y <= 1; y++ {
			if (x == 0 && y == 0) || (x != 0 && y != 0) {
				continue
			}
			nx, ny := i+x, j+y
			if nx < 0 || nx >= len(topology) || ny < 0 || ny >= len(topology[0]) {
				continue
			}
			if topology[nx][ny] == elevation+1 {
				score += calculateScore(topology, nx, ny, elevation+1, visited)
			}
		}
	}
	return score
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	topology := parseInput(input)

	score := 0

	for i, row := range topology {
		for j, value := range row {
			if value == 0 {
				visited := make(map[[2]int]bool)
				score += calculateScore(topology, i, j, 0, visited)
			}
		}
	}

	return fmt.Sprintf("%v", score)
}

func calculateRating(topology [][]int, i, j, elevation int) int {
	if elevation == 9 {
		return 1
	}

	score := 0

	for x := -1; x <= 1; x++ {
		for y := -1; y <= 1; y++ {
			if (x == 0 && y == 0) || (x != 0 && y != 0) {
				continue
			}
			nx, ny := i+x, j+y
			if nx < 0 || nx >= len(topology) || ny < 0 || ny >= len(topology[0]) {
				continue
			}
			if topology[nx][ny] == elevation+1 {
				score += calculateRating(topology, nx, ny, elevation+1)
			}
		}
	}
	return score
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	topology := parseInput(input)

	score := 0

	for i, row := range topology {
		for j, value := range row {
			if value == 0 {
				score += calculateRating(topology, i, j, 0)
			}
		}
	}

	return fmt.Sprintf("%v", score)
}
