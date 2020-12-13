package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

fun run15(input: String): Solution {
    val lines = input.trim().lines()
    val discs = lines.mapIndexed { n, d ->
        d.split(" ").let { seg -> { t: Int -> (seg[11].dropLast(1).toInt() + n + 1 + t) % seg[3].toInt() } }
    }.toMutableList()
    val res1 = timing(discs)

    discs += { t -> (lines.size + 1 + t) % 11 }
    val res2 = timing(discs)

    return Solution(res1, res2)
}

private fun timing(discs: List<(Int) -> Int>) =
    (0..Int.MAX_VALUE)
        .first { t ->
            discs.all { it(t) == 0 }
        }
        .toULong()
