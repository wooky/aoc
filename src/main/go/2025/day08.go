package y2025

import (
	"maps"
	"math"
	"slices"
	"strconv"
	"strings"
)

type Coord3 struct {
	x, y, z int
}

type Distance struct {
	distance int
	from, to Coord3
}

func day08(input string) (string, string) {
	// Collect coords
	var coords []Coord3
	for line := range strings.Lines(input) {
		xyz := strings.Split(strings.TrimSuffix(line, "\n"), ",")
		x, _ := strconv.Atoi(xyz[0])
		y, _ := strconv.Atoi(xyz[1])
		z, _ := strconv.Atoi(xyz[2])
		coords = append(coords, Coord3{x, y, z})
	}

	// Get top closest coord pairs
	var distances [1000]Distance
	for i := range distances {
		distances[i].distance = math.MaxInt
	}
	for i := 0; i < len(coords)-1; i++ {
		from := coords[i]
		for _, to := range coords[i+1:] {
			dx, dy, dz := from.x-to.x, from.y-to.y, from.z-to.z
			d := dx*dx + dy*dy + dz*dz
			lastDistance := distances[len(distances)-1]
			if d <= lastDistance.distance {
				distances[len(distances)-1] = Distance{d, from, to}
				slices.SortFunc(distances[:], func(a, b Distance) int {
					return a.distance - b.distance
				})
			}
		}
	}

	// Map each coord to junction box
	boxes := make(map[Coord3]int)
	nextBox := 0
	for _, distance := range distances {
		// Upsert box of from coord
		box, fromFound := boxes[distance.from]
		if !fromFound {
			box = nextBox
			nextBox++
		}
		boxes[distance.from] = box

		// Add to coord, or join to's box to from's box
		toBox, toFound := boxes[distance.to]
		if toFound && toBox != box {
			for k, v := range boxes {
				if v == toBox {
					boxes[k] = box
				}
			}
		} else {
			boxes[distance.to] = box
		}
	}

	// Count how many coords exist for each junction box
	boxCount := make(map[int]int)
	for _, box := range boxes {
		boxCount[box]++
	}

	// Get product of top 3 box counts
	sortedBoxCounts := slices.Sorted(maps.Values(boxCount))
	slices.Reverse(sortedBoxCounts)
	s1 := sortedBoxCounts[0] * sortedBoxCounts[1] * sortedBoxCounts[2]

	return strconv.Itoa(s1), ""
}
