package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution
import platform.posix.pow
import kotlin.math.floor
import kotlin.math.log2

fun run19(inputUntrimmed: String): Solution {
    val input = inputUntrimmed.trim().toInt()

    // PART 1 ALGORITHM
    // Result resets to 1 whenever input is 2^x + 1
    // Result increments by 2
    val lower1 = pow(2.0, floor(log2(input.toDouble()))).toInt()
    val res1 = ((input - lower1) * 2 + 1).toULong()

    // PART 2 ALGORITHM
    // Result resets to 1 whenever input is 3^x + 1
    // Assume input lies in the range (3^x + 1)..3^(x+1), the range having n numbers
    // For the first n/2 numbers, the result increments by 1
    // For the last n/2 numbers, the result increments by 2

    val power2 = floor(log2(input.toDouble()) / log2(3.0))
    val lower2 = pow(3.0, power2).toInt()
    val upper2 = pow(3.0, power2 + 1).toInt()
    val mid2 = (upper2 + lower2) / 2
    val res2 =
        if (input <= mid2) {
            input - lower2
        } else {
            (mid2 / 2) + (input - mid2) * 2
        }.toULong()

    return Solution(res1, res2)
}
