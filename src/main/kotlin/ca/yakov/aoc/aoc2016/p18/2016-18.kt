package ca.yakov.aoc.aoc2016.p18

fun main(args: Array<String>) {
    var row = input
    var safe = input.count{ it == '.' }
    for(i in 2..400000) {
        row =
                ".$row."
                        .windowed(3)
                        .map{ when(it) {
                            "^^.", ".^^", "^..", "..^" -> '^'
                            else -> '.'
                        } }
                        .joinToString("")
        safe += row.count{ it == '.' }
        if(i == 40) {
            println(safe)
        }
    }
    println(safe)
}