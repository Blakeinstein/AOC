package day_04

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

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	count := 0
	n := len(input)
	m := len(input[0])

	for i := 0; i < n; i++ {
		for j := 0; j <= m-4; j++ {
			if input[i][j:j+4] == "XMAS" || input[i][j:j+4] == "SAMX" {
				count++
			}
		}
	}

	for i := 0; i <= n-4; i++ {
		for j := 0; j < m; j++ {
			if input[i][j] == 'X' && input[i+1][j] == 'M' && input[i+2][j] == 'A' && input[i+3][j] == 'S' {
				count++
			}
			if input[i][j] == 'S' && input[i+1][j] == 'A' && input[i+2][j] == 'M' && input[i+3][j] == 'X' {
				count++
			}
		}
	}

	for i := 0; i <= n-4; i++ {
		for j := 0; j <= m-4; j++ {
			if input[i][j] == 'X' && input[i+1][j+1] == 'M' && input[i+2][j+2] == 'A' && input[i+3][j+3] == 'S' {
				count++
			}
			if input[i][j] == 'S' && input[i+1][j+1] == 'A' && input[i+2][j+2] == 'M' && input[i+3][j+3] == 'X' {
				count++
			}
		}
		for j := 3; j < m; j++ {
			if input[i][j] == 'X' && input[i+1][j-1] == 'M' && input[i+2][j-2] == 'A' && input[i+3][j-3] == 'S' {
				count++
			}
			if input[i][j] == 'S' && input[i+1][j-1] == 'A' && input[i+2][j-2] == 'M' && input[i+3][j-3] == 'X' {
				count++
			}
		}
	}

	return fmt.Sprintf("%d", count)
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	count := 0

	n := len(input)
	m := len(input[0])

	for i := 1; i < n-1; i++ {
		for j := 1; j < m-1; j++ {
			if input[i][j] != 'A' {
				continue
			}

			if (input[i-1][j-1] == 'M' && input[i+1][j+1] == 'S') || (input[i+1][j+1] == 'M' && input[i-1][j-1] == 'S') {
				if (input[i+1][j-1] == 'M' && input[i-1][j+1] == 'S') || (input[i-1][j+1] == 'M' && input[i+1][j-1] == 'S') {
					count++
				}
			}

		}
	}

	return fmt.Sprintf("%d", count)
}
