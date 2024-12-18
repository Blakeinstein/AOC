package day_18

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

type Item struct {
	point    Point
	priority int
	index    int
}

const W = 70
const H = 70
const N = 1024

type Point struct {
	x int
	y int
}

func parseInput(input []string) (mems []Point) {
	mems = make([]Point, len(input))
	for i, line := range input {
		var x, y int
		fmt.Sscanf(line, "%d,%d", &x, &y)
		mems[i] = Point{x, y}
	}
	return
}

func getSteps(mems []Point, n int) int {
	start := Point{x: 0, y: 0}
	goal := Point{x: W, y: H}
	steps := 0

	walls := make(map[Point]bool)
	for _, wall := range mems[:n] {
		walls[wall] = true
	}

	front := map[Point]bool{start: true}
	seen := map[Point]bool{start: true}

	directions := []Point{
		{x: 1, y: 0},
		{x: -1, y: 0},
		{x: 0, y: 1},
		{x: 0, y: -1},
	}

	for len(front) > 0 {
		newFront := make(map[Point]bool)
		steps++

		for pos := range front {
			for _, d := range directions {
				newPos := Point{x: pos.x + d.x, y: pos.y + d.y}

				if newPos == goal {
					return steps
				}

				if newPos.x < 0 || newPos.x > W || newPos.y < 0 || newPos.y > H {
					continue
				}

				if walls[newPos] || seen[newPos] {
					continue
				}

				seen[newPos] = true
				newFront[newPos] = true
			}
		}

		front = newFront
	}
	return 0
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	mems := parseInput(input)

	return fmt.Sprintf("%d", getSteps(mems, N))
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	mems := parseInput(input)

	left, right := N, len(mems)

	for right-left > 1 {
		mid := (left + right) / 2
		if getSteps(mems, mid) > 0 {
			left = mid
		} else {
			right = mid
		}
	}

	return fmt.Sprintf("%d,%d", mems[left].x, mems[left].y)
}
