package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

fun run18(input: String): Solution {
    var row = input.trim()
    var safe = row.count { it == '.' }
    var safe40 = 0uL
    for (i in 2..400000) {
        row =
            ".$row."
                .windowed(3)
                .map {
                    when (it) {
                        "^^.", ".^^", "^..", "..^" -> '^'
                        else -> '.'
                    }
                }
                .joinToString("")
        safe += row.count { it == '.' }
        if (i == 40) {
            safe40 = safe.toULong()
        }
    }
    return Solution(safe40, safe.toULong())
}
