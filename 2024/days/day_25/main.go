package day_25

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

type Point struct {
	x, y int
}

func (p Point) Add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y}
}

var (
	DOWN   = Point{0, 1}
	UP     = Point{0, -1}
	ORIGIN = Point{0, 0}
)

func parseInput(input []string) (res [][]string) {
	res = make([][]string, 0)
	curr := make([]string, 0)
	for _, line := range input {
		if line == "" {
			res = append(res, curr)
			curr = make([]string, 0)
		} else {
			curr = append(curr, line)
		}
	}
	res = append(res, curr)

	return
}

func parseGrid(lines []string) [][]rune {
	grid := make([][]rune, len(lines))
	for i, line := range lines {
		grid[i] = []rune(line)
	}
	return grid
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	locks := []int{}
	keys := []int{}
	result := 0

	blocks := parseInput(input)
	for _, block := range blocks {
		grid := parseGrid(block)
		heights := 0

		if grid[ORIGIN.y][ORIGIN.x] == '#' {
			for x := 0; x < 5; x++ {
				position := Point{x, 1}
				for position.y < len(grid) && grid[position.y][position.x] == '#' {
					position = position.Add(DOWN)
				}
				heights = (heights << 4) + (position.y - 1)
			}
			locks = append(locks, heights)
		} else {
			for x := 0; x < 5; x++ {
				position := Point{x, 5}
				for position.y >= 0 && grid[position.y][position.x] == '#' {
					position = position.Add(UP)
				}
				heights = (heights << 4) + (5 - position.y)
			}
			keys = append(keys, heights)
		}
	}

	for _, lock := range locks {
		for _, key := range keys {
			if (lock+key+0x22222)&0x88888 == 0 {
				result++
			}
		}
	}

	return fmt.Sprint(result)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	return ""
}
