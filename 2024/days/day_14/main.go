package day_14

import (
	"fmt"
	"strconv"

	stats "github.com/dgryski/go-onlinestats"
	"github.com/samber/lo"
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

type Robot struct {
	p Point
	v Point
}

func parseInput(input []string) []Robot {
	var robots []Robot
	for _, line := range input {
		var r Robot
		fmt.Sscanf(line, "p=%d,%d v=%d,%d", &r.p.x, &r.p.y, &r.v.x, &r.v.y)
		robots = append(robots, r)
	}
	return robots
}

const w = 101
const h = 103

func wrapPoint(p, v, t, dim int) int {
	next := (p + t*v) % dim
	if next < 0 {
		next += dim
	}
	return next
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {

	ex := w / 2
	ey := h / 2

	q1 := 0
	q2 := 0
	q3 := 0
	q4 := 0

	robots := parseInput(input)

	// count := map[Point]int{}

	for _, robot := range robots {
		x := wrapPoint(robot.p.x, robot.v.x, 100, w)
		y := wrapPoint(robot.p.y, robot.v.y, 100, h)

		if x == ex || y == ey {
			continue
		}

		if x > ex {
			if y > ey {
				q4++
			} else {
				q2++
			}
		} else {
			if y > ey {
				q3++
			} else {
				q1++
			}
		}
	}

	return strconv.Itoa(q1 * q2 * q3 * q4)
}

func isAnomaly(positions []Point, xCdf, yCdf []float64) bool {
	xs := lo.Map(positions, func(p Point, _ int) float64 { return float64(p.x) })
	ys := lo.Map(positions, func(p Point, _ int) float64 { return float64(p.y) })

	xP := stats.KS(xs, xCdf)
	yP := stats.KS(ys, yCdf)

	return xP < 0.01 && yP < 0.01
}

func printFrame(frame int, robots []Robot) {
	positions := map[Point]bool{}
	for _, robot := range robots {
		x := wrapPoint(robot.p.x, robot.v.x, frame, w)
		y := wrapPoint(robot.p.y, robot.v.y, frame, h)

		positions[Point{x, y}] = true
	}

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			if positions[Point{x, y}] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func makeRange(min, max int) []float64 {
	a := make([]float64, max-min+1)
	for i := range a {
		a[i] = float64(min + i)
	}
	return a
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	w := 101
	h := 103

	robots := parseInput(input)
	frame := 0

	xCdf := makeRange(0, w)
	yCdf := makeRange(0, h)

	for i := 0; i < 10000; i++ {
		positions := []Point{}
		for _, robot := range robots {
			x := wrapPoint(robot.p.x, robot.v.x, frame, w)
			y := wrapPoint(robot.p.y, robot.v.y, frame, h)
			positions = append(positions, Point{x, y})
		}

		if isAnomaly(positions, xCdf, yCdf) {
			fmt.Println("Anomaly detected at frame: ", frame)
			printFrame(frame, robots)
		}

		frame++
	}

	return ""

}
