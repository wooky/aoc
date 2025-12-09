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

	s1, s2 := 0, 0
	for i, p1 := range coords[:len(coords)-1] {
	out:
		for _, p2 := range coords[i+2:] {
			if p1.X == p2.X || p1.Y == p2.Y {
				continue
			}
			area := (abs(p1.X-p2.X) + 1) * (abs(p1.Y-p2.Y) + 1)
			if area > s1 {
				s1 = area
			}
			if area > s2 {
				// Get corner coords
				p3, p4 := lib.Coord{X: p1.X, Y: p2.Y}, lib.Coord{X: p2.X, Y: p1.Y}
				var tl, tr, bl, br lib.Coord
				if p1.X < p2.X {
					if p1.Y < p2.Y {
						tl, tr, bl, br = p1, p4, p3, p2
					} else {
						tl, tr, bl, br = p3, p2, p1, p4
					}
				} else {
					if p1.Y < p2.Y {
						tl, tr, bl, br = p4, p1, p2, p3
					} else {
						tl, tr, bl, br = p2, p3, p4, p1
					}
				}

				var tlFound, trFound, blFound, brFound bool
				for j, pa := range coords[:len(coords)-1] {
					// Check that no point falls inside the rectangle
					if pa.X > tl.X && pa.Y > tl.Y && pa.X < br.X && pa.Y < br.Y {
						continue out
					}

					// Check that no line is drawn through the rectangle
					pb := coords[j+1]
					if pa.X == pb.X && pa.X > tl.X && pa.X < br.X && min(pa.Y, pb.Y) <= tl.Y && max(pa.Y, pb.Y) >= br.Y {
						continue out
					}
					if pa.Y > tl.Y && pa.Y < br.Y && min(pa.X, pb.X) <= tl.X && max(pa.X, pb.X) >= br.X {
						continue out
					}

					// Check that any point falls outside the rectangle
					if pa.X <= tl.X && pa.Y <= tl.Y {
						tlFound = true
					} else if pa.X >= tr.X && pa.Y <= tr.Y {
						trFound = true
					} else if pa.X <= bl.X && pa.Y >= bl.Y {
						blFound = true
					} else if pa.X >= br.X && pa.Y >= br.Y {
						brFound = true
					}
				}
				if tlFound && trFound && blFound && brFound {
					s2 = area
				}
			}
		}
	}

	return strconv.Itoa(s1), strconv.Itoa(s2)
}

func abs(res int) int {
	if res < 0 {
		return -res
	}
	return res
}
