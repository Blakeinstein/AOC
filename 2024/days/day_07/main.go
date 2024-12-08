package day_07

import (
	"fmt"
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

type Equation struct {
	goal     int64
	operands []int64
}

func parseInput(input []string) []Equation {
	equations := make([]Equation, len(input))

	for i, line := range input {
		parts := strings.Split(line, ": ")

		goal, _ := strconv.ParseInt(parts[0], 10, 64)
		operandStrings := strings.Split(parts[1], " ")
		operands := make([]int64, len(operandStrings))
		for j, operandStr := range operandStrings {
			operand, _ := strconv.ParseInt(operandStr, 10, 64)
			operands[j] = operand
		}
		equations[i] = Equation{
			goal:     goal,
			operands: operands,
		}
	}
	return equations
}

func recurse2op(equation Equation, index int, sum int64) bool {
	if equation.goal == sum {
		return true
	}

	if index >= len(equation.operands) {
		return false
	}

	return recurse2op(equation, index+1, sum+equation.operands[index]) || recurse2op(equation, index+1, sum*equation.operands[index])
}

func isTrue2op(equation Equation) bool {
	return recurse2op(equation, 0, 0)
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	equations := parseInput(input)

	result := int64(0)

	for _, equation := range equations {
		if isTrue2op(equation) {
			result += equation.goal
		}
	}

	return fmt.Sprintf("%d", result)
}

func orOp(a, b int64) int64 {
	if a == 0 {
		return b
	}

	cat := fmt.Sprintf("%d%d", a, b)

	res, _ := strconv.ParseInt(cat, 10, 64)

	return res
}

func recurse3op(equation Equation, index int, sum int64) bool {

	if sum > equation.goal {
		return false
	}

	if index >= len(equation.operands) {
		return equation.goal == sum
	}

	return (recurse3op(equation, index+1, orOp(sum, equation.operands[index])) ||
		recurse3op(equation, index+1, sum+equation.operands[index]) ||
		recurse3op(equation, index+1, sum*equation.operands[index]))
}

func isTrue3op(equation Equation) bool {
	return recurse3op(equation, 0, 0)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	equations := parseInput(input)

	result := int64(0)

	for _, equation := range equations {
		if isTrue3op(equation) {
			result += equation.goal
		}
	}

	return fmt.Sprintf("%d", result)
}
