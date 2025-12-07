package y2025

import (
	"strconv"
	"strings"
)

func day07(input string) (string, string) {
	s1, s2 := 0, 0
	beams := make(map[int]int)

	skip := true
	for line := range strings.Lines(input) {
		skip = !skip
		if skip {
			continue
		}
		if len(beams) == 0 {
			beams[strings.IndexByte(line, 'S')] = 1
			continue
		}

		newBeams := make(map[int]int)
		for beam, count := range beams {
			if line[beam] == '^' {
				s1++
				newBeams[beam-1] += count
				newBeams[beam+1] += count
			} else {
				newBeams[beam] += count
			}
		}
		beams = newBeams
	}

	for _, count := range beams {
		s2 += count
	}

	return strconv.Itoa(s1), strconv.Itoa(s2)
}
