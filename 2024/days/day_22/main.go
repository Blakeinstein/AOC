package day_22

import (
	"fmt"
	"strconv"
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

func parseInput(input []string) (buyerSecrets []int) {
	buyerSecrets = make([]int, len(input))
	for i, line := range input {
		buyerSecrets[i], _ = strconv.Atoi(line)
	}
	return
}

func mix(secret, next int) int {
	return (secret ^ next)
}

func prune(secret int) int {
	return secret & 0xFFFFFF
}

func getNextSecret(secret int) int {
	result := secret
	result = prune(mix(result, result<<6))
	result = prune(mix(result, result>>5))
	result = prune(mix(result, result<<11))
	return result
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	sum := 0
	buyerSecrets := parseInput(input)
	for _, init := range buyerSecrets {
		secret := init
		for i := 0; i < 2000; i++ {
			secret = getNextSecret(secret)
		}
		sum += secret
	}
	return strconv.Itoa(sum)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	buyerSecrets := parseInput(input)

	changeMap := map[[4]int]int{}
	for _, secret := range buyerSecrets {
		changes := make([]int, 2000)
		currentPrice := secret % 10

		currentChangeMap := map[[4]int]int{}

		for i := range 2000 {
			secret = getNextSecret(secret)
			newPrice := secret % 10
			diff := newPrice - currentPrice

			changes[i] = diff
			if i >= 3 {
				ch := [4]int(changes[i-3 : i+1])
				if _, ok := currentChangeMap[ch]; !ok {
					currentChangeMap[ch] = newPrice
				}
			}

			currentPrice = newPrice
		}
		for changeSeq, bananas := range currentChangeMap {
			if _, ok := changeMap[changeSeq]; !ok {
				changeMap[changeSeq] = 0
			}
			changeMap[changeSeq] += bananas
		}
	}

	// find max value in sequences map
	maxBananas := -1
	for _, v := range changeMap {
		if v > maxBananas {
			maxBananas = v
		}
	}
	return strconv.Itoa(maxBananas)
}
