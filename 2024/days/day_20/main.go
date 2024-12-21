package day_20

import (
	"fmt"
	"math"
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

type Shortcut struct {
	start Point
	end   Offset
}

type Offset struct {
	point    Point
	distance int
}

type Point struct {
	x int
	y int
}

const WALL = '#'
const EMPTY = '.'
const START = 'S'
const END = 'E'

func parseInput(input []string) (start Point, end Point, walls map[Point]int) {
	walls = make(map[Point]int)
	for y, line := range input {
		for x, char := range line {
			switch char {
			case START:
				start = Point{x, y}
			case END:
				end = Point{x, y}
			case WALL:
				walls[Point{x, y}]++
			}
		}
	}
	return
}

func findRoute(start, end Point, walls map[Point]int) map[Point]int {
	queue := []Point{start}
	visited := make(map[Point]int)

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		visited[current] = len(visited)

		if current == end {
			return visited
		}

		for _, offset := range getOffsets(current, 1) {
			if _, found := visited[offset.point]; found {
				continue
			}

			if _, found := walls[offset.point]; found {
				continue
			}

			queue = append(queue, offset.point)
		}
	}

	panic("Cannot find route")
}

func findShortcuts(route, walls map[Point]int, radius int) map[int]int {
	// Cheat!
	shortcuts := make(map[Shortcut]int)
	for current, step := range route {

		offsets := getOffsets(current, radius)
		for _, offset := range offsets {
			routeStep, inRoute := route[offset.point]
			if inRoute {
				saving := routeStep - step - offset.distance
				if saving > 0 {
					shortcuts[Shortcut{current, offset}] = saving
				}
			}
		}
	}

	// Transform to summary.
	result := make(map[int]int)
	for _, saving := range shortcuts {
		result[saving]++
	}

	return result
}

func getOffsets(from Point, radius int) []Offset {
	result := []Offset{}

	for y := radius * -1; y <= radius; y++ {
		for x := radius * -1; x <= radius; x++ {
			candidatePoint := Point{from.x + x, from.y + y}
			candidate := Offset{
				candidatePoint,
				getDistance(from, candidatePoint),
			}

			if candidate.distance > 0 && candidate.distance <= radius {
				result = append(result, candidate)
			}
		}
	}

	return result
}

func getDistance(from, until Point) int {
	xDistance := math.Abs(float64(from.x - until.x))
	yDistance := math.Abs(float64(from.y - until.y))
	return int(xDistance + yDistance)
}

func findOptimalShortcuts(start, end Point, walls map[Point]int, cheatDuration int) (greatShortcuts int) {
	route := findRoute(start, end, walls)
	savings := findShortcuts(route, walls, cheatDuration)

	found := 0
	for k := 0; found < len(savings); k++ {
		if v, ok := savings[k]; ok {
			found++
			if k >= 100 {
				greatShortcuts += v
			}
		}
	}

	return
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	start, end, walls := parseInput(input)

	return fmt.Sprintf("%d", findOptimalShortcuts(start, end, walls, 2))
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	start, end, walls := parseInput(input)

	return fmt.Sprintf("%d", findOptimalShortcuts(start, end, walls, 20))
}
