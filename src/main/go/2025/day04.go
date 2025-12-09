package y2025

import (
	lib "main/lib"
	"maps"
	"strconv"
	"strings"
)

func day04(input string) (string, string) {
	grid := make(map[lib.Coord]any)
	for y, line := range strings.Split(input, "\n") {
		for x, c := range line {
			if c == '@' {
				grid[lib.Coord{X: x, Y: y}] = nil
			}
		}
	}
	s1, s2 := remove(grid), 0
	for removed := s1; removed != 0; {
		s2 += removed
		removed = remove(grid)
	}

	return strconv.Itoa(s1), strconv.Itoa(s2)
}

func remove(grid map[lib.Coord]any) int {
	removed := 0
	old := maps.Clone(grid)
	for coord := range old {
		if accessible(old, coord) {
			delete(grid, coord)
			removed++
		}
	}
	return removed
}

func accessible(grid map[lib.Coord]any, coord lib.Coord) bool {
	count := 0
	for y := coord.Y - 1; y <= coord.Y+1; y++ {
		for x := coord.X - 1; x <= coord.X+1; x++ {
			if x == coord.X && y == coord.Y {
				continue
			}
			_, present := grid[lib.Coord{X: x, Y: y}]
			if present {
				count++
			}
		}
	}
	return count < 4
}
