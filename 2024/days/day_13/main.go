package day_13

import (
	"fmt"
	"math"
	"strconv"
	"strings"
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
	x float64
	y float64
}

type ClawMachine struct {
	A     Point
	B     Point
	prize Point
}

func parseInput(input []string, offset float64) []ClawMachine {
	var clawMachines []ClawMachine
	for i := 0; i < len(input); i += 4 {
		aParts := strings.Split(input[i], ": ")[1]
		bParts := strings.Split(input[i+1], ": ")[1]
		prizeParts := strings.Split(input[i+2], ": ")[1]

		aCoords := strings.Split(aParts, ", ")
		bCoords := strings.Split(bParts, ", ")
		prizeCoords := strings.Split(prizeParts, ", ")

		aX, _ := strconv.Atoi(strings.Split(aCoords[0], "+")[1])
		aY, _ := strconv.Atoi(strings.Split(aCoords[1], "+")[1])
		bX, _ := strconv.Atoi(strings.Split(bCoords[0], "+")[1])
		bY, _ := strconv.Atoi(strings.Split(bCoords[1], "+")[1])
		prizeX, _ := strconv.Atoi(strings.Split(prizeCoords[0], "=")[1])
		prizeY, _ := strconv.Atoi(strings.Split(prizeCoords[1], "=")[1])

		clawMachines = append(clawMachines, ClawMachine{
			A:     Point{x: float64(aX), y: float64(aY)},
			B:     Point{x: float64(bX), y: float64(bY)},
			prize: Point{x: float64(prizeX) + offset, y: float64(prizeY) + offset},
		})
	}
	return clawMachines
}

func isInt(f float64) bool {
	return math.Abs(f-float64(int(f))) < 1e-9
}

func countTokenCost(machines []ClawMachine) string {
	tokenCost := 0
	for _, m := range machines {
		na := (m.prize.x*m.B.y - m.prize.y*m.B.x) / (m.A.x*m.B.y - m.A.y*m.B.x)
		nb := (m.prize.y*m.A.x - m.prize.x*m.A.y) / (m.A.x*m.B.y - m.A.y*m.B.x)

		if isInt(na) && isInt(nb) {
			tokenCost += int(na*3 + nb)
		}
	}

	return strconv.Itoa(tokenCost)
}

func Part1(input []string) string {
	machines := parseInput(input, 0)

	return countTokenCost(machines)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	machines := parseInput(input, 10000000000000)

	return countTokenCost(machines)
}
