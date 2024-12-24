package day_24

import (
	"fmt"
	"sort"
	"strings"
)

type Gate struct {
	in1, in2, op, out string
}

// Run function of the daily challenge
func Run(input []string, mode int) {
	if mode == 1 || mode == 3 {
		fmt.Printf("Part one: %v\n", Part1(input))
	}
	if mode == 2 || mode == 3 {
		fmt.Printf("Part two: %v\n", Part2(input))
	}
}

func parseInput(input []string) (init map[string]bool, gates []Gate) {
	init = make(map[string]bool)
	gates = make([]Gate, 0)

	i := 0
	for ; i < len(input); i++ {
		if input[i] == "" {
			break
		}

		line := input[i]
		parts := strings.Split(line, ": ")
		init[parts[0]] = parts[1] == "1"
	}

	for i++; i < len(input); i++ {
		gate := Gate{}
		fmt.Sscanf(input[i], "%s %s %s -> %s", &gate.in1, &gate.op, &gate.in2, &gate.out)
		gates = append(gates, gate)
	}

	return
}

func contains(registers map[string]bool, key string) bool {
	_, ok := registers[key]
	return ok
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	registers, gates := parseInput(input)

	for len(gates) > 0 {
		gate := gates[0]
		gates = gates[1:]

		if !contains(registers, gate.in1) || !contains(registers, gate.in2) {
			gates = append(gates, gate)
			continue
		}

		switch gate.op {
		case "AND":
			registers[gate.out] = registers[gate.in1] && registers[gate.in2]
		case "OR":
			registers[gate.out] = registers[gate.in1] || registers[gate.in2]
		case "XOR":
			registers[gate.out] = registers[gate.in1] != registers[gate.in2]
		}
	}

	result := make([]string, 0)

	for key := range registers {
		if key[0] == 'z' {
			result = append(result, key)
		}
	}

	sort.Strings(result)

	decimalValue := 0
	for i, key := range result {
		if registers[key] {
			decimalValue += 1 << i
		}
	}

	return fmt.Sprintf("%d", decimalValue)
}

// find searches for a gate matching the given inputs and operator.
func find(a, b, operator string, gates []Gate) string {
	for _, gate := range gates {
		if gate.op == operator && ((gate.in1 == a && gate.in2 == b) || (gate.in1 == b && gate.in2 == a)) {
			return gate.out
		}
	}
	return ""
}

// Part2 solves the second part of the exercise
// Inspired by https://github.com/ayoubzulfiqar/advent-of-code/blob/main/2024/Go/Day24/part_2.go
func Part2(input []string) string {
	_, gates := parseInput(input)
	var swapped []string
	var c0 string

	for i := 0; i < 45; i++ {
		n := fmt.Sprintf("%02d", i)
		var m1, n1, r1, z1, c1 string

		// Half adder logic
		m1 = find("x"+n, "y"+n, "XOR", gates)
		n1 = find("x"+n, "y"+n, "AND", gates)

		if c0 != "" {
			r1 = find(c0, m1, "AND", gates)
			if r1 == "" {
				m1, n1 = n1, m1
				swapped = append(swapped, m1, n1)
				r1 = find(c0, m1, "AND", gates)
			}

			z1 = find(c0, m1, "XOR", gates)

			if strings.HasPrefix(m1, "z") {
				m1, z1 = z1, m1
				swapped = append(swapped, m1, z1)
			}

			if strings.HasPrefix(n1, "z") {
				n1, z1 = z1, n1
				swapped = append(swapped, n1, z1)
			}

			if strings.HasPrefix(r1, "z") {
				r1, z1 = z1, r1
				swapped = append(swapped, r1, z1)
			}

			c1 = find(r1, n1, "OR", gates)
		}

		if strings.HasPrefix(c1, "z") && c1 != "z45" {
			c1, z1 = z1, c1
			swapped = append(swapped, c1, z1)
		}

		if c0 == "" {
			c0 = n1
		} else {
			c0 = c1
		}
	}

	// Sort and join swapped wires
	sort.Strings(swapped)
	return strings.Join(swapped, ",")
}
