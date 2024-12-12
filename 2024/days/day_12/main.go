package day_12

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

var directions = []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

func hasVisited(p Point, visited map[Point]bool) bool {
	_, ok := visited[p]
	return ok
}

func addPoint(p1, p2 Point) Point {
	return Point{p1.x + p2.x, p1.y + p2.y}
}

func isInBounds(p Point, w, h int) bool {
	return p.x >= 0 && p.x < w && p.y >= 0 && p.y < h
}

func fenceCost(p Point, garden []string, visited map[Point]bool) int {
	fenceCount := 0
	area := 0
	flower := garden[p.y][p.x]
	h := len(garden)
	w := len(garden[0])

	queue := []Point{p}
	for len(queue) > 0 {
		p = queue[0]
		queue = queue[1:]
		if hasVisited(p, visited) {
			continue
		}
		area++
		visited[p] = true

		for _, d := range directions {
			np := Point{p.x + d.x, p.y + d.y}
			if !isInBounds(np, w, h) || garden[np.y][np.x] != flower {
				fenceCount++
				continue
			}
			queue = append(queue, np)
		}
	}
	return area * fenceCount
}

// Part1 solves the first part of the exercise
func Part1(garden []string) string {
	h := len(garden)
	w := len(garden[0])
	visited := make(map[Point]bool)
	totalCost := 0

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			if !hasVisited(Point{x, y}, visited) {
				totalCost += fenceCost(Point{x, y}, garden, visited)
			}
		}
	}

	return fmt.Sprintf("%d", totalCost)
}

func checkCorners(garden []string, pos Point) int {
	count := 0
	flower := garden[pos.y][pos.x]
	x, y := pos.x, pos.y

	if x == 0 && y == 0 {
		count += 1
	}

	if x == 0 && y == len(garden)-1 {
		count += 1
	}

	if x == len(garden[0])-1 && y == len(garden)-1 {
		count += 1
	}

	if x == len(garden[0])-1 && y == 0 {
		count += 1
	}

	// top left outside corner
	// ##   __   |#
	// #O   #O   |O
	if (x > 0 && y > 0 && garden[y][x-1] != flower && garden[y-1][x] != flower) ||
		(x > 0 && y == 0 && garden[y][x-1] != flower) || (x == 0 && y > 0 && garden[y-1][x] != flower) {
		count += 1
	}

	// top left inside corner
	// OO
	// O#
	if x < len(garden[0])-1 && y < len(garden)-1 && garden[y][x+1] == flower && garden[y+1][x] == flower && garden[y+1][x+1] != flower {
		count += 1
	}

	// top right outside corner
	// ##   __    #|
	// O#   O#    O|
	if (x < len(garden[0])-1 && y > 0 && garden[y][x+1] != flower && garden[y-1][x] != flower) ||
		(x < len(garden[0])-1 && y == 0 && garden[y][x+1] != flower) || (x == len(garden[0])-1 && y > 0 && garden[y-1][x] != flower) {
		count += 1
	}

	// top right inside corner
	// OO
	// #O
	if x > 0 && y < len(garden)-1 && garden[y][x-1] == flower && garden[y+1][x] == flower && garden[y+1][x-1] != flower {
		count += 1
	}

	// bottom left outside corner
	// #O   #O    |O
	// ##   --    |#
	if (x > 0 && y < len(garden)-1 && garden[y][x-1] != flower && garden[y+1][x] != flower) ||
		(x > 0 && y == len(garden)-1 && garden[y][x-1] != flower) || (x == 0 && y < len(garden)-1 && garden[y+1][x] != flower) {
		count += 1
	}

	// bottom left inside corner
	// O#
	// OO
	if x < len(garden[0])-1 && y > 0 && garden[y][x+1] == flower && garden[y-1][x] == flower && garden[y-1][x+1] != flower {
		count += 1
	}

	// bottom right outside corner
	// O#   O#    O|
	// ##   --    #|
	if (x < len(garden[0])-1 && y < len(garden)-1 && garden[y][x+1] != flower && garden[y+1][x] != flower) ||
		(x < len(garden[0])-1 && y == len(garden)-1 && garden[y][x+1] != flower) || (x == len(garden[0])-1 && y < len(garden)-1 && garden[y+1][x] != flower) {
		count += 1
	}

	// bottom right inside corner
	// #O
	// OO
	if x > 0 && y > 0 && garden[y][x-1] == flower && garden[y-1][x] == flower && garden[y-1][x-1] != flower {
		count += 1
	}
	return count
}

func fenceBulkCost(p Point, garden []string, visited map[Point]bool) int {
	area := 0
	sides := 0
	flower := garden[p.y][p.x]
	h := len(garden)
	w := len(garden[0])

	queue := []Point{p}
	for len(queue) > 0 {
		p = queue[0]
		queue = queue[1:]
		if hasVisited(p, visited) {
			continue
		}
		area++
		visited[p] = true

		for _, d := range directions {
			np := addPoint(p, d)
			if isInBounds(np, w, h) && garden[np.y][np.x] == flower {
				queue = append(queue, np)
			}
		}

		sides += checkCorners(garden, p)

	}
	return area * sides
}

// Part2 solves the second part of the exercise
func Part2(garden []string) string {
	h := len(garden)
	w := len(garden[0])
	visited := make(map[Point]bool)
	totalCost := 0

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			if !hasVisited(Point{x, y}, visited) {
				totalCost += fenceBulkCost(Point{x, y}, garden, visited)
			}
		}
	}

	return fmt.Sprintf("%d", totalCost)
}
