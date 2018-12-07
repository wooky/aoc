package ca.yakov.aoc.aoc2016.p13

import kotlin.math.abs

const val input = 1352

data class Coord(val x: Int, val y: Int)
val start = Coord(1, 1)
val goal = Coord(31, 39)

class Stats(coord: Coord, val gScore: Int) {
    val fScore = gScore + abs(coord.x-goal.x) + abs(coord.y-goal.y)
}

val spaces = mutableMapOf<Coord, Boolean>()
fun isSpace(coord: Coord) =
        spaces.getOrPut(coord) {
            Integer
                    .toBinaryString(coord.x*coord.x + 3*coord.x + 2*coord.x*coord.y + coord.y + coord.y*coord.y + input)
                    .count{ it == '1' } % 2 == 0
        }
