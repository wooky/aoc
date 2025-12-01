package y2025

import (
	"strconv"
	"strings"
)

func day01(input string) (string, string) {
	dial := 50
	s1, s2 := 0, 0
	for line := range strings.Lines(input) {
		starts_at_0 := dial == 0
		delta, _ := strconv.Atoi(strings.TrimSuffix(line[1:], "\n"))
		switch line[0] {
		case 'L':
			dial -= delta
		case 'R':
			dial += delta
		}

		if dial >= 100 {
			clicks := dial / 100
			dial -= clicks * 100
			s2 += clicks
		} else if dial <= 0 {
			clicks := -(dial / 100) + 1
			dial += clicks * 100

			if dial == 100 {
				dial = 0
			}

			s2 += clicks
			if starts_at_0 {
				s2--
			}
		}

		if dial == 0 {
			s1++
		}
	}
	return strconv.Itoa(s1), strconv.Itoa(s2)
}
