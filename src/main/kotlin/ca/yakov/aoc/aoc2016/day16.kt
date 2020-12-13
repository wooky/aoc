package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

fun run16(input: String): Solution {
    val inputTrimmed = input.trim()
    return Solution(
        checkMe(inputTrimmed, 272),
        checkMe(inputTrimmed, 35651584)
    )
}

private fun checkMe(input: String, length: Int): ULong {
    var a = input
    while (a.length < length) {
        val b = a.reversed().map { if (it == '0') '1' else '0' }.joinToString("")
        a += "0$b"
    }
    var checksum = a.substring(0, length)
    while (checksum.length % 2 == 0) {
        checksum = checksum.chunked(2) { if (it[0] == it[1]) 1 else 0 }.joinToString("")
    }
    return checksum.toULong()
}
