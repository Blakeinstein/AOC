package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"

	"github.com/blakeinstein/AOC/2024/internal"
)

// main entry point
// The --day parameter is required to choose which daily challenge should be executed.
// The --input parameter is also required, and it points to the input file that should be used for the challenge.
// The --mode parameter specifies which part of the challenge should be executed:
// - 1: only the first part
// - 2: only the second part
// - 3 or empty: both parts
func main() {
	d := flag.String("day", "", "day ID to execute")
	m := flag.String("mode", "3", "running mode")
	flag.Parse()

	if d == nil {
		fmt.Println("missing day parameter")
		os.Exit(1)
	}

	day, err := strconv.Atoi(*d)
	if err != nil {
		fmt.Println("couldn't parse day")
		os.Exit(1)
	}

	mode, err := strconv.Atoi(*m)
	if err != nil {
		fmt.Println("incorrect mode")
		os.Exit(1)
	}

	inputPath := fmt.Sprintf("input/day%02d.txt", day)
	internal.RunChallenge(day, inputPath, mode)
}
