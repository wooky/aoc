package main

/*
struct AocSolution {
	const char *S1;
	const char *S2;
};
*/
import "C"

type Solution struct {
	S1 string
	S2 string
}

//export Run
func Run(input *C.char, year int, day int) C.struct_AocSolution {
	// file := C.GoString(input)
	solution := Solution{"Hello", "World"}
	return C.struct_AocSolution{C.CString(solution.S1), C.CString(solution.S2)}
}

func main() {}
