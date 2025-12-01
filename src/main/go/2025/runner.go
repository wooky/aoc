package y2025

func Run(input string, day int) (string, string) {
	switch day {
	case 1:
		return day01(input)
	default:
		panic("Invalid day for 2025")
	}
}
