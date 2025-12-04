package y2025

import (
	"maps"
	"strconv"
	"strings"
)

type Coord struct {
	x, y int
}

func day04(input string) (string, string) {
	grid := make(map[Coord]any)
	for y, line := range strings.Split(input, "\n") {
		for x, c := range line {
			if c == '@' {
				grid[Coord{x, y}] = nil
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

func remove(grid map[Coord]any) int {
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

func accessible(grid map[Coord]any, coord Coord) bool {
	count := 0
	for y := coord.y - 1; y <= coord.y+1; y++ {
		for x := coord.x - 1; x <= coord.x+1; x++ {
			if x == coord.x && y == coord.y {
				continue
			}
			_, present := grid[Coord{x, y}]
			if present {
				count++
			}
		}
	}
	return count < 4
}
