package y2025

import (
	"strconv"
	"strings"
)

type PathInfo struct {
	neither, dac, fft, both int
}

type Reactor struct {
	flow     map[string][]string
	pathInfo map[string]PathInfo
}

func day11(input string) (string, string) {
	flow := make(map[string][]string)
	for line := range strings.Lines(input) {
		tokens := strings.Split(strings.TrimSuffix(line, "\n"), " ")
		flow[tokens[0][:len(tokens[0])-1]] = tokens[1:]
	}
	s1 := Reactor{flow, make(map[string]PathInfo)}.calcPaths("you").neither
	s2 := Reactor{flow, make(map[string]PathInfo)}.calcPaths("svr").both
	return strconv.Itoa(s1), strconv.Itoa(s2)
}

func (r Reactor) calcPaths(curr string) PathInfo {
	{
		pi, found := r.pathInfo[curr]
		if found {
			return pi
		}
	}
	if curr == "out" {
		r.pathInfo[curr] = PathInfo{neither: 1}
		return r.pathInfo[curr]
	}
	pi := PathInfo{}
	for _, next := range r.flow[curr] {
		nextPi := r.calcPaths(next)
		pi.neither += nextPi.neither
		pi.dac += nextPi.dac
		pi.fft += nextPi.fft
		pi.both += nextPi.both
	}
	switch curr {
	case "dac":
		pi.both += pi.fft
		pi.dac += pi.neither
		pi.fft = 0
		pi.neither = 0
	case "fft":
		pi.both += pi.dac
		pi.fft += pi.neither
		pi.dac = 0
		pi.neither = 0
	}
	r.pathInfo[curr] = pi
	return pi
}
