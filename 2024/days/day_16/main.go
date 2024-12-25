package day_16

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

type Point struct {
	x int
	y int
}

type Node struct {
	p Point
	d Point
}

type Input struct {
	m [][]rune
	s Point
	e Point
}

func parseInput(input []string) Input {
	var m [][]rune
	var s Point
	var e Point
	for y, line := range input {
		var row []rune
		for x, r := range line {
			row = append(row, r)
			if r == 'S' {
				s = Point{x, y}
			}
			if r == 'E' {
				e = Point{x, y}
			}
		}
		m = append(m, row)
	}
	return Input{m, s, e}
}

var directions = []Point{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}

type State struct {
	node Node
	cost int
	path []Point
}

func getNeighbours(n Node) (nbrs []Node) {
	nbrs = make([]Node, 0, 4)
	currDir, currPos := n.d, n.p
	opp := Point{-currDir.x, -currDir.y}

	for _, dir := range directions {
		if dir == opp {
			continue
		}
		nP := Point{x: currPos.x + dir.x, y: currPos.y + dir.y}
		nbrs = append(nbrs, Node{nP, dir})
	}
	return
}

func bfs(inp Input) (int, int) {
	queue := []State{{Node{inp.s, directions[1]}, 0, []Point{inp.s}}}
	visited := make(map[Node]int)
	minCost := math.MaxInt
	mapPath := make(map[Point]int)

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current.cost > minCost {
			continue
		}

		if current.node.p == inp.e {
			if current.cost <= minCost {
				minCost = current.cost
				for _, p := range current.path {
					mapPath[p] = current.cost
				}
			}
			continue
		}

		for _, nP := range getNeighbours(current.node) {
			if nP.p.x < 0 || nP.p.x >= len(inp.m[0]) || nP.p.y < 0 || nP.p.y >= len(inp.m) || inp.m[nP.p.y][nP.p.x] == '#' {
				continue
			}

			nextCost := current.cost + 1
			if nP.d != current.node.d {
				nextCost += 1000
			}

			if v, ok := visited[nP]; ok {
				if v < nextCost {
					continue
				}
			}

			visited[nP] = nextCost
			nPath := make([]Point, len(current.path))
			copy(nPath, current.path)
			queue = append(queue, State{
				nP,
				nextCost,
				append(nPath, nP.p),
			})
		}
	}

	if minCost == math.MaxInt {
		return 0, 0
	}

	count := 0

	for _, cost := range mapPath {
		if cost == minCost {
			count++
		}
	}

	return minCost, count
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	inp := parseInput(input)

	minCost, _ := bfs(inp)
	return fmt.Sprintf("%d", minCost)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	inp := parseInput(input)

	_, count := bfs(inp)
	return fmt.Sprintf("%d", count)
}
