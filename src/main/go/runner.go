package main

/*
struct AocSolution {
	const char *S1;
	const char *S2;
};
*/
import "C"
import (
	y2025 "main/2025"
)

//export Run
func Run(input *C.char, year int, day int) C.struct_AocSolution {
	file := C.GoString(input)
	var s1, s2 string
	switch year {
	case 2025:
		s1, s2 = y2025.Run(file, day)
	default:
		panic("Invalid year")
	}
	return C.struct_AocSolution{C.CString(s1), C.CString(s2)}
}

func main() {}
