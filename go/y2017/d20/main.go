package main

import (
	"fmt"
)

func Part1(fileName string) int {
	realData := parseFile(fileName)
	return Solve1(realData)
}

func Part2(fileName string) int {
	realData := parseFile(fileName)
	return Solve2(realData)
}

func main() {
	const file = "y2017/d20/real.txt"

	result1 := Part1(file)
	fmt.Println("Part 1:", result1)

	result2 := Part2(file)
	fmt.Println("Part 2:", result2)
}
