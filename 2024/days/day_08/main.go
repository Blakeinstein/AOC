package day_08

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
	x int
	y int
}

type Node struct {
	point Point
	char  rune
}

func parseInput(input []string) []Node {
	towers := []Node{}

	for y, line := range input {
		for x, char := range line {
			if char != '.' {
				towers = append(towers, Node{Point{x, y}, char})
			}
		}
	}

	return towers
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	height := len(input)
	width := len(input[0])

	towers := parseInput(input)

	antinodes := make(map[Point]bool)

	for i, node1 := range towers {
		for _, node2 := range towers[i+1:] {
			if node1.char != node2.char {
				continue
			}

			p1 := node1.point
			p2 := node2.point

			dx := 2 * (p2.x - p1.x)
			dy := 2 * (p2.y - p1.y)
			// 2

			px := p2.x - dx
			py := p2.y - dy

			if px >= 0 && px < width && py >= 0 && py < height {
				antinodes[Point{px, py}] = true
			}

			px = p1.x + dx
			py = p1.y + dy

			if px >= 0 && px < width && py >= 0 && py < height {
				antinodes[Point{px, py}] = true
			}
		}
	}

	return fmt.Sprintf("%d", len(antinodes))
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	height := len(input)
	width := len(input[0])

	towers := parseInput(input)

	antinodes := make(map[Point]bool)

	for i, node1 := range towers {
		for _, node2 := range towers[i+1:] {
			if node1.char != node2.char {
				continue
			}

			antinodes[node1.point] = true
			antinodes[node2.point] = true

			p1 := node1.point
			p2 := node2.point

			dx := (p2.x - p1.x)
			dy := (p2.y - p1.y)
			// 2

			px := p2.x - 2*dx
			py := p2.y - 2*dy

			for px >= 0 && px < width && py >= 0 && py < height {
				antinodes[Point{px, py}] = true
				px -= dx
				py -= dy
			}

			px = p1.x + 2*dx
			py = p1.y + 2*dy

			for px >= 0 && px < width && py >= 0 && py < height {
				antinodes[Point{px, py}] = true
				py += dy
				px += dx
			}
		}
	}

	return fmt.Sprintf("%d", len(antinodes))
}
