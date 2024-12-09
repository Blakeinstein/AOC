package day_09

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

// Part1 solves the first part of the exercise
func Part1(input []string) string {
	line := input[0]

	blocks := []int{}
	isBlock := true
	blockCount := 0

	for _, char := range line {
		count, err := strconv.Atoi(string(char))
		if err != nil {
			fmt.Println("Error converting rune to int:", err)
			return ""
		}

		if isBlock {
			for i := 0; i < count; i++ {
				blocks = append(blocks, blockCount)
			}
			blockCount++
		} else {
			for i := 0; i < count; i++ {
				blocks = append(blocks, -1)
			}
		}

		isBlock = !isBlock
	}

	idx := 0
	popIdx := len(blocks) - 1

	for idx < popIdx {
		if blocks[popIdx] == -1 {
			popIdx--
			continue
		}
		if blocks[idx] != -1 {
			idx++
			continue
		}
		blocks[idx], blocks[popIdx] = blocks[popIdx], blocks[idx]
	}

	sum := 0

	for i, b := range blocks {
		if b == -1 {
			continue
		}
		sum += i * b
	}

	return strconv.Itoa(sum)
}

type File struct {
	size     int
	position int
}

// Part2 solves the second part of the exercise
func Part2(input []string) string {
	line := input[0]

	files := []File{}
	spaces := []File{}

	isFile := true

	pos := 0

	for _, char := range line {
		digit, err := strconv.Atoi(string(char))

		if err != nil {
			fmt.Println("Error converting rune to int:", err)
			return ""
		}

		if isFile {
			files = append(files, File{digit, pos})
		} else {
			spaces = append(spaces, File{digit, pos})
		}
		pos += digit

		isFile = !isFile
	}

	idx := len(files) - 1

	for idx >= 1 {
		for i := range spaces {
			if spaces[i].position > files[idx].position {
				break
			}
			if spaces[i].size >= files[idx].size {
				spaces[i].size -= files[idx].size
				files[idx].position = spaces[i].position
				spaces[i].position += files[idx].size
				break
			}
		}
		idx--
	}

	sum := 0

	for i, f := range files {
		sum += f.size * i * (2*f.position + f.size - 1) / 2
	}
	return fmt.Sprintf("%d", sum)
}
