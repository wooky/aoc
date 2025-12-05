package y2025

import (
	"strconv"
	"strings"
)

type Range struct {
	from, to uint64
}

func day05(input string) (string, string) {
	nilRange := Range{0, 0}
	s1 := 0
	makeRanges := true
	var ranges []Range
out:
	for line := range strings.Lines(input) {
		line = strings.TrimSuffix(line, "\n")
		if makeRanges {
			if line == "" {
				makeRanges = false
			} else {
				fromTo := strings.Split(line, "-")
				from, _ := strconv.ParseUint(fromTo[0], 10, 64)
				to, _ := strconv.ParseUint(fromTo[1], 10, 64)
				newRange := Range{from, to}

				for i, r := range ranges {
					if r == nilRange {
						continue
					}
					if newRange.from >= r.from && newRange.to <= r.to {
						continue out
					}
					if newRange.from <= r.from && newRange.to >= r.to {
						ranges[i] = nilRange
					} else if newRange.from <= r.to && newRange.to >= r.from {
						ranges[i] = nilRange
						if r.from < newRange.from {
							newRange.from = r.from
						}
						if r.to > newRange.to {
							newRange.to = r.to
						}
					}
				}

				ranges = append(ranges, newRange)
			}
		} else {
			n, _ := strconv.ParseUint(line, 10, 64)
			for _, r := range ranges {
				if r.from <= n && n <= r.to {
					s1++
					continue out
				}
			}
		}
	}

	s2 := uint64(0)
	for _, r := range ranges {
		if r != nilRange {
			s2 += r.to - r.from + 1
		}
	}

	return strconv.Itoa(s1), strconv.FormatUint(s2, 10)
}
