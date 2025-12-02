package y2025

import (
	"strconv"
	"strings"
)

func day02(input string) (string, string) {
	var s1, s2 uint64 = 0, 0
	for rng := range strings.SplitSeq(input, ",") {
		from_to := strings.Split(rng, "-")
		v1, v2 := invalids(from_to[0], from_to[1])
		s1 += v1
		s2 += v2
	}
	return strconv.FormatUint(s1, 10), strconv.FormatUint(s2, 10)
}

func invalids(from, to string) (uint64, uint64) {
	s1, s2 := make(map[uint64]struct{}), make(map[uint64]struct{})
	for i := 1; ; i++ {
		seq := strconv.Itoa(i)
		if len(seq)*2 > len(to) {
			break
		}
		for id, repeat := seq+seq, 2; len(id) < len(to) || (len(id) == len(to) && strings.Compare(id, to) <= 0); id, repeat = id+seq, repeat+1 {
			if len(from) > len(id) || (len(from) == len(id) && strings.Compare(from, id) == 1) {
				continue
			}
			n, _ := strconv.ParseUint(id, 10, 64)
			s2[n] = struct{}{}
			if repeat%2 == 0 {
				s1[n] = struct{}{}
			}
		}
	}
	var ss1, ss2 uint64 = 0, 0
	for v := range s1 {
		ss1 += v
	}
	for v := range s2 {
		ss2 += v
	}
	return ss1, ss2
}
