package day_06

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

type Position struct {
	x int
	y int
}

var directions = [4]Position{
	{0, -1},
	{1, 0},
	{0, 1},
	{-1, 0},
}

type Guard struct {
	position  Position
	direction int
}

const obstacle = '#'

func parseInput(input []string) [][]rune {
	grid := make([][]rune, len(input))
	for i, line := range input {
		grid[i] = []rune(line)
	}
	return grid
}

const startChar = '^'

func findInitialPosition(grid [][]rune) (Position, error) {
	for i, row := range grid {
		for j, cell := range row {
			if cell == startChar {
				return Position{j, i}, nil
			}
		}
	}
	return Position{}, fmt.Errorf("initial position not found")
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	grid := parseInput(input)

	width := len(grid)
	height := len(grid[0])

	var uniquePosition = make(map[Position]bool)

	initialPosition, err := findInitialPosition(grid)

	if err != nil {
		print(err)
		return ""
	}

	guard := Guard{initialPosition, 0}

	for {
		uniquePosition[guard.position] = true
		nextX := guard.position.x + directions[guard.direction].x
		nextY := guard.position.y + directions[guard.direction].y

		if !(nextX >= 0 && nextX < width && nextY >= 0 && nextY < height) {
			break
		}

		if grid[nextY][nextX] == obstacle {
			guard.direction = (guard.direction + 1) % 4
			continue
		}

		guard.position.x = nextX
		guard.position.y = nextY
	}

	return fmt.Sprintf("%d", len(uniquePosition))
}

func isLoop(initalPos Position, grid [][]rune) bool {
	history := make(map[Guard]bool)
	width := len(grid)
	height := len(grid[0])

	guard := Guard{initalPos, 0}

	for {
		if _, ok := history[guard]; ok {
			return true
		}

		history[guard] = true
		nextX := guard.position.x + directions[guard.direction].x
		nextY := guard.position.y + directions[guard.direction].y

		if !(nextX >= 0 && nextX < width && nextY >= 0 && nextY < height) {
			return false
		}

		if grid[nextY][nextX] == obstacle {
			guard.direction = (guard.direction + 1) % 4
			continue
		}

		guard.position.x = nextX
		guard.position.y = nextY
	}
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	grid := parseInput(input)

	initialPos, _ := findInitialPosition(grid)

	loopCount := 0

	for i, row := range grid {
		for j, cell := range row {
			if cell == obstacle {
				continue
			}
			grid[i][j] = obstacle
			if isLoop(initialPos, grid) {
				loopCount++
			}
			grid[i][j] = cell
		}
	}

	return fmt.Sprintf("%d", loopCount)
}
