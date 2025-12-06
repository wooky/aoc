package y2025

import (
	"regexp"
	"strconv"
	"strings"
)

func day06(input string) (string, string) {
	s1 := uint64(0)
	tokenizer := regexp.MustCompile(`\S+`)
	lines := strings.Split(input, "\n")
	var numbers [][]uint64
	for _, line := range lines[:len(lines)-1] {
		var l []uint64
		for _, s := range tokenizer.FindAllString(line, -1) {
			n, _ := strconv.ParseUint(s, 10, 64)
			l = append(l, n)
		}
		numbers = append(numbers, l)
	}
	for idx, op := range tokenizer.FindAllString(lines[len(lines)-1], -1) {
		if op == "+" {
			res := uint64(0)
			for _, n := range numbers {
				res += n[idx]
			}
			s1 += res
		} else {
			res := uint64(1)
			for _, n := range numbers {
				res *= n[idx]
			}
			s1 += res
		}
	}
	return strconv.FormatUint(s1, 10), ""
}
