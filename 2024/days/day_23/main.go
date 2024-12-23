package day_23

import (
	"fmt"
	"sort"
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

func parseInput(input []string) (graph map[string][]string) {
	graph = make(map[string][]string)
	for _, line := range input {
		nodes := strings.Split(line, "-")
		graph[nodes[0]] = append(graph[nodes[0]], nodes[1])
		graph[nodes[1]] = append(graph[nodes[1]], nodes[0])
	}
	return
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	graph := parseInput(input)

	count := 0
	for node1, v1 := range graph {
		for _, node2 := range v1 {
			for _, node3 := range graph[node2] {
				if node3 == node1 {
					continue
				}
				for _, node4 := range graph[node3] {
					if node4 == node1 {
						if node1[0] == 't' || node2[0] == 't' || node3[0] == 't' {
							count++
						}
					}
				}
			}
		}
	}

	return fmt.Sprintf("%v", count/6)
}

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	graph := parseInput(input)

	var maxClique []string

	var findClique func([]string, []string)
	findClique = func(potentialClique, remainingNodes []string) {
		if len(remainingNodes) == 0 {
			if len(potentialClique) > len(maxClique) {
				maxClique = append([]string{}, potentialClique...)
			}
			return
		}

		for i, node := range remainingNodes {
			newPotentialClique := append(potentialClique, node)
			newRemainingNodes := []string{}
			for _, neighbor := range graph[node] {
				if contains(remainingNodes[i+1:], neighbor) {
					newRemainingNodes = append(newRemainingNodes, neighbor)
				}
			}
			findClique(newPotentialClique, newRemainingNodes)
		}
	}

	nodes := make([]string, 0, len(graph))
	for node := range graph {
		nodes = append(nodes, node)
	}
	findClique([]string{}, nodes)

	sort.Strings(maxClique)
	return strings.Join(maxClique, ",")
}
