package y2025

import (
	"container/list"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

func day10(input string) (string, string) {
	s1, s2 := 0, 0
	for line := range strings.Lines(input) {
		tokens := strings.Split(strings.TrimSuffix(line, "\n"), " ")

		var buttonGroups [][]int
		for _, token := range tokens[1 : len(tokens)-1] {
			var buttonGroup []int
			for s := range strings.SplitSeq(token[1:len(token)-1], ",") {
				b, _ := strconv.Atoi(s)
				buttonGroup = append(buttonGroup, b)
			}
			buttonGroups = append(buttonGroups, buttonGroup)
		}

		s1 += processInit(tokens[0], buttonGroups)
		// s2 += processJoltages(tokens[len(tokens)-1], buttonGroups)
	}
	return strconv.Itoa(s1), strconv.Itoa(s2)
}

func processInit(indicatorLights string, buttonGroups [][]int) int {
	type Operation struct {
		steps int
		mask  int
	}

	expectedMask := 0
	for i, c := range indicatorLights[1 : len(indicatorLights)-1] {
		if c == '#' {
			expectedMask |= (1 << i)
		}
	}

	masksSeen := make(map[int]any)
	operations := list.New()
	operations.PushBack(Operation{0, 0})
	for e := operations.Front(); ; _, e = operations.Remove(e), operations.Front() {
		op := e.Value.(Operation)
		if op.mask == expectedMask {
			return op.steps
		}

		_, found := masksSeen[op.mask]
		if found {
			continue
		}
		masksSeen[op.mask] = nil

		for _, buttonGroup := range buttonGroups {
			newMask := op.mask
			for _, b := range buttonGroup {
				newMask ^= (1 << b)
			}
			operations.PushBack(Operation{steps: op.steps + 1, mask: newMask})
		}
	}
}

func processJoltages(joltageRequirement string, buttonGroupsArr [][]int) int {
	fmt.Println(joltageRequirement)
	type Operation struct {
		steps    int
		joltages []int
	}

	buttonGroups := make(map[int][]int)
	for idx, buttonGroup := range buttonGroupsArr {
		buttonGroups[idx] = buttonGroup
	}

	var expectedJoltages []int
	for s := range strings.SplitSeq(joltageRequirement[1:len(joltageRequirement)-1], ",") {
		j, _ := strconv.Atoi(s)
		expectedJoltages = append(expectedJoltages, j)
	}

	initialOperation := Operation{0, make([]int, len(expectedJoltages))}
	for {
		singleButtons := make(map[int]int)
		for idx, buttonGroup := range buttonGroups {
			for _, b := range buttonGroup {
				_, found := singleButtons[b]
				if found {
					singleButtons[b] = -1
				} else {
					singleButtons[b] = idx
				}
			}
		}

		foundButtonGroup := false
		for b, buttonGroupIdx := range singleButtons {
			if buttonGroupIdx != -1 {
				mult := expectedJoltages[b]
				initialOperation.steps += mult
				for _, bb := range buttonGroups[buttonGroupIdx] {
					initialOperation.joltages[bb] += mult
				}
				delete(buttonGroups, buttonGroupIdx)
				foundButtonGroup = true
			}
		}

		if !foundButtonGroup {
			break
		}
	}
	fmt.Println(initialOperation)

	operations := list.New()
	operations.PushBack(initialOperation)
	for e := operations.Front(); ; _, e = operations.Remove(e), operations.Front() {
		op := e.Value.(Operation)
		if slices.Equal(op.joltages, expectedJoltages) {
			return op.steps
		}

	out:
		for _, buttonGroup := range buttonGroups {
			newJoltages := slices.Clone(op.joltages)
			for _, b := range buttonGroup {
				if newJoltages[b] >= expectedJoltages[b] {
					continue out
				}
				newJoltages[b]++
			}
			operations.PushBack(Operation{steps: op.steps + 1, joltages: newJoltages})
		}
	}
}
