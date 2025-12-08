package y2025

import (
	"maps"
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

	// Get and sort coord pair distances
	var distances []Distance
	for i := 0; i < len(coords)-1; i++ {
		from := coords[i]
		for _, to := range coords[i+1:] {
			dx, dy, dz := from.x-to.x, from.y-to.y, from.z-to.z
			d := dx*dx + dy*dy + dz*dz
			distances = append(distances, Distance{d, from, to})
		}
	}
	slices.SortFunc(distances, func(a, b Distance) int { return a.distance - b.distance })

	var s1 int
	{
		// Map closest 1000 coords to junction box
		boxes, _ := linkBoxes(distances[:1000], len(coords))

		// Count how many coords exist for each junction box
		boxCount := make(map[int]int)
		for _, box := range boxes {
			boxCount[box]++
		}

		// Get product of top 3 box counts
		sortedBoxCounts := slices.Sorted(maps.Values(boxCount))
		slices.Reverse(sortedBoxCounts)
		s1 = sortedBoxCounts[0] * sortedBoxCounts[1] * sortedBoxCounts[2]
	}

	_, s2 := linkBoxes(distances, len(coords))

	return strconv.Itoa(s1), strconv.Itoa(s2)
}

func linkBoxes(distances []Distance, coordCount int) (map[Coord3]int, int) {
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
			inCircuit := 0
			for k, v := range boxes {
				switch v {
				case toBox:
					boxes[k] = box
					inCircuit++
				case box:
					inCircuit++
				}
			}
			if inCircuit == coordCount {
				return boxes, distance.from.x * distance.to.x
			}
		} else {
			boxes[distance.to] = box
		}
	}

	return boxes, 0
}
