package day_17

import (
	"fmt"
	"slices"
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

type Registers struct {
	a  uint64
	b  uint64
	c  uint64
	ip int
}

func getOpd(r *Registers, opd rune) uint64 {
	switch opd {
	case '4':
		return r.a
	case '5':
		return r.b
	case '6':
		return r.c
	case '7':
		fmt.Println("saw 7")
		return 1
	default:
		return uint64(opd - '0')
	}
}

func performOperation(r *Registers, op, opd rune, stdout *[]int) {
	switch op {
	case '0':
		r.a = r.a >> getOpd(r, opd)
	case '1':
		r.b = r.b ^ uint64(opd-'0')
	case '2':
		r.b = getOpd(r, opd) & 7
	case '3':
		if r.a != 0 {
			r.ip = int(opd - '0')
			return
		}
	case '4':
		r.b = r.b ^ r.c
	case '5':
		*stdout = append(*stdout, int(getOpd(r, opd)&7))
	case '6':
		r.b = r.a >> getOpd(r, opd)
	case '7':
		r.c = r.a >> getOpd(r, opd)
	}
	r.ip += 2
}

func parseInput(input []string) (r Registers, ops []rune) {
	ra, _ := strconv.Atoi(strings.Split(input[0], ": ")[1])
	rb, _ := strconv.Atoi(strings.Split(input[1], ": ")[1])
	rc, _ := strconv.Atoi(strings.Split(input[2], ": ")[1])

	r = Registers{uint64(ra), uint64(rb), uint64(rc), 0}
	ops = lo.Map(strings.Split(strings.Split(input[4], ": ")[1], ","), func(s string, _ int) rune { return []rune(s)[0] })

	return
}

func perfOps(r *Registers, ops []rune) (stdout []int) {
	for r.ip < len(ops) {
		performOperation(r, ops[r.ip], ops[r.ip+1], &stdout)
	}
	return
}

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	r, ops := parseInput(input)
	stdout := perfOps(&r, ops)
	return strings.Trim(strings.Replace(fmt.Sprint(stdout), " ", ",", -1), "[]")
}

func seedOps(ops []rune, seed uint64) []int {
	r := Registers{seed, 0, 0, 0}
	return perfOps(&r, ops)
}

func findMirror(ops []rune) (seed uint64) {
	expected_out := lo.Map(ops, func(op rune, _ int) int { return int(op - '0') })
	for itr := len(ops) - 1; itr >= 0; itr-- {
		seed <<= 3
		for !slices.Equal(seedOps(ops, seed), expected_out[itr:]) {
			seed++
		}
	}
	return
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	_, ops := parseInput(input)
	seed := findMirror(ops)
	return fmt.Sprintf("%v", seed)
}
