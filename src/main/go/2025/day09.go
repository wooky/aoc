package y2025

import (
	"main/lib"
	"strconv"
	"strings"
)

func day09(input string) (string, string) {
	var coords []lib.Coord
	for line := range strings.Lines(input) {
		xy := strings.Split(strings.Trim(line, "\n"), ",")
		x, _ := strconv.Atoi(xy[0])
		y, _ := strconv.Atoi(xy[1])
		coords = append(coords, lib.Coord{X: x, Y: y})
	}

	s1 := 0
	for i, p1 := range coords[:len(coords)-1] {
		for _, p2 := range coords[i+2:] {
			area := abs(p1.X-p2.X+1) * abs(p1.Y-p2.Y+1)
			if area > s1 {
				s1 = area
			}
		}
	}

	return strconv.Itoa(s1), ""
}

func abs(res int) int {
	if res < 0 {
		return -res
	}
	return res
}
