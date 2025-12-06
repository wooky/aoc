package y2025

import (
	"regexp"
	"strconv"
	"strings"
)

func day06(input string) (string, string) {
	s1, s2 := uint64(0), uint64(0)
	tokenizer := regexp.MustCompile(`[+*]\s+`)
	lines := strings.Split(input, "\n")
	ops := lines[len(lines)-1]

	for _, fromTo := range tokenizer.FindAllStringIndex(ops, -1) {
		from, to := fromTo[0], fromTo[1]
		var horNums, vertNums []uint64

		for _, line := range lines[:len(lines)-1] {
			num, _ := strconv.ParseUint(strings.TrimSpace(line[from:to]), 10, 64)
			horNums = append(horNums, num)
		}

		for idx := from; idx < to; idx++ {
			n := uint64(0)
			for _, line := range lines[:len(lines)-1] {
				c := line[idx]
				if c != ' ' {
					n = n*10 + uint64(c-'0')
				}
			}
			if n != 0 {
				vertNums = append(vertNums, n)
			}
		}

		if ops[from] == '+' {
			r1, r2 := uint64(0), uint64(0)
			for _, n := range horNums {
				r1 += n
			}
			for _, n := range vertNums {
				r2 += n
			}
			s1 += r1
			s2 += r2
		} else {
			r1, r2 := uint64(1), uint64(1)
			for _, n := range horNums {
				r1 *= n
			}
			for _, n := range vertNums {
				r2 *= n
			}
			s1 += r1
			s2 += r2
		}
	}

	return strconv.FormatUint(s1, 10), strconv.FormatUint(s2, 10)
}
