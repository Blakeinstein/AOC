package day_22_test

import (
	"testing"

	"github.com/blakeinstein/AOC/2024/days/day_22"
	"github.com/blakeinstein/AOC/2024/internal"
)

func TestPartOne(t *testing.T) {
	t.Parallel()

	input := internal.LoadInputLines("input_test.txt")
	expectedResult := internal.LoadFirstInputLine("solution_1.txt")
	result := day_22.Part1(input)

	if result != expectedResult {
		t.Errorf("expected result was %s, but got %s instead", expectedResult, result)
	}
}

func TestPartTwo(t *testing.T) {
	t.Parallel()

	input := internal.LoadInputLines("input_test.txt")
	expectedResult := internal.LoadFirstInputLine("solution_2.txt")
	result := day_22.Part2(input)

	if result != expectedResult {
		t.Errorf("expected result was %s, but got %s instead", expectedResult, result)
	}
}
