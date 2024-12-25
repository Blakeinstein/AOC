package day_05

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

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

type Order struct {
	before int
	after  int
}

func parseInput(input []string) ([]Order, [][]int) {
	var orders []Order
	var updates [][]int

	idx := 0

	for idx < len(input) {
		line := input[idx]
		if line == "" {
			idx++
			break
		}

		parts := strings.Split(line, "|")
		before, _ := strconv.Atoi(parts[0])
		after, _ := strconv.Atoi(parts[1])
		orders = append(orders, Order{before: before, after: after})
		idx++
	}

	for idx < len(input) {
		line := input[idx]
		parts := strings.Split(line, ",")
		update := lo.Map(
			parts,
			func(part string, _ int) int { num, _ := strconv.Atoi(part); return num },
		)
		updates = append(updates, update)
		idx++
	}

	return orders, updates
}

func isValid(update []int, orders []Order) int {
	valueToIndex := make(map[int]int)
	for i, val := range update {
		valueToIndex[val] = i
	}

	middle := update[len(update)/2]

	for _, order := range orders {

		i, beforeExists := valueToIndex[order.before]
		j, afterExists := valueToIndex[order.after]

		if !beforeExists || !afterExists {
			continue
		}

		if i > j {
			return 0
		}
	}
	return middle
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	orders, updates := parseInput(input)
	res := 0

	for _, update := range updates {
		middle := isValid(update, orders)
		res += middle
	}

	return fmt.Sprintf("%d", res)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	orders, updates := parseInput(input)

	orderSet := make(map[Order]bool)
	for _, order := range orders {
		orderSet[order] = true
	}

	res := 0

	for _, update := range updates {
		if isValid(update, orders) > 0 {
			continue
		}

		sort.Slice(update, func(a, b int) bool {
			_, exists := orderSet[Order{before: update[a], after: update[b]}]
			return exists
		})

		middle := update[len(update)/2]
		res += middle
	}

	return fmt.Sprintf("%d", res)
}
