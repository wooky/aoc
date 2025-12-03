package y2025

import (
	"math"
	"slices"
	"strconv"
	"strings"
)

func day03(input string) (string, string) {
	var s1, s2 int64 = 0, 0
	for bank := range strings.Lines(input) {
		bank = strings.TrimSuffix(bank, "\n")
		s1 += joltage(bank, 2)
		s2 += joltage(bank, 12)
	}
	return strconv.FormatInt(s1, 10), strconv.FormatInt(s2, 10)
}

func joltage(chunk string, size int) int64 {
	if len(chunk) < size {
		return -1
	}
	if len(chunk) == size {
		res, _ := strconv.ParseInt(chunk, 10, 64)
		return res
	}
	if size == 1 {
		return int64(slices.Max([]byte(chunk)) - '0')
	}

	var res int64 = -1
	var c byte
	for c = '9'; res == -1; c-- {
		_, next, _ := strings.Cut(chunk, string(c))
		res = joltage(next, size-1)
	}
	return int64(c+1-'0')*int64(math.Pow10(size-1)) + res
}
