package y2025

func Run(input string, day int) (string, string) {
	switch day {
	case 1:
		return day01(input)
	case 2:
		return day02(input)
	case 3:
		return day03(input)
	case 4:
		return day04(input)
	case 5:
		return day05(input)
	default:
		panic("Invalid day for 2025")
	}
}
