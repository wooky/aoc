package ca.yakov.aoc.aoc2016.p13

// TODO convert to A*, not just a DIY job
fun main(args: Array<String>) {
    for(y in 0..49) {
        for(x in 0..41) {
            if(x == 31 && y == 39) {
                print("O")
            }
            else {
                val wat = x*x + 3*x + 2*x*y + y + y*y + input
                val space = Integer.toBinaryString(wat).count{ it == '1' } % 2 == 0
                print(if(space) '.' else '#')
            }
        }
        println()
    }
}