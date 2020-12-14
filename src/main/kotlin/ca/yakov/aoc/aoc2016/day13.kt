package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution
import kotlin.math.abs

private val start = Coord(1, 1)
private val goal = Coord(31, 39)
private val spaces = mutableMapOf<Coord, Boolean>()

private data class Coord(val x: Int, val y: Int)
private class Node(val coord: Coord, val dist: Int)
private class Stats(coord: Coord, val gScore: Int) {
    val fScore = gScore + abs(coord.x - goal.x) + abs(coord.y - goal.y)
}

fun run13(input: String): Solution {
    val inputInt = input.trim().toInt()
    val closedSet = mutableSetOf(start)
    val openMap = mutableMapOf<Coord, Stats>()
    val queue = mutableListOf(Node(start, 0))

    while (queue.isNotEmpty()) {
        val node = queue.removeFirst()
        val coord = node.coord
        closedSet.add(coord)
        if (node.dist == 50) {
            openMap += coord to Stats(coord, 50)
            continue
        }

        listOf(
            Coord(coord.x - 1, coord.y),
            Coord(coord.x + 1, coord.y),
            Coord(coord.x, coord.y - 1),
            Coord(coord.x, coord.y + 1)
        )
            .filter { it !in closedSet && it.x >= 0 && it.y >= 0 && isSpace(it, inputInt) }
            .forEach {
                closedSet += it
                queue += Node(it, node.dist + 1)
            }
    }

    val visited = closedSet.size.toULong()
    val leastSteps = aStar(openMap, closedSet, inputInt)
    return Solution(leastSteps, visited)
}

private fun aStar(openMap: MutableMap<Coord, Stats>, closedSet: MutableSet<Coord>, input: Int): ULong {
    while (true) {
        val node = openMap.minByOrNull { (_, v) -> v.fScore }!!
        val coord = node.key
        if (coord == goal) {
            return node.value.gScore.toULong()
        }

        val tentativeGscore = node.value.gScore + 1
        openMap -= coord
        closedSet += coord

        listOf(
            Coord(coord.x - 1, coord.y),
            Coord(coord.x + 1, coord.y),
            Coord(coord.x, coord.y - 1),
            Coord(coord.x, coord.y + 1)
        )
            .filter {
                it !in closedSet && it.x >= 0 && it.y >= 0 &&
                        openMap[it]?.let { it.gScore > tentativeGscore } != false &&
                        isSpace(it, input)
            }
            .forEach {
                openMap += it to Stats(it, tentativeGscore)
            }
    }
}

private fun isSpace(coord: Coord, input: Int) =
    spaces.getOrPut(coord) {
        (coord.x * coord.x + 3 * coord.x + 2 * coord.x * coord.y + coord.y + coord.y * coord.y + input)
            .countOneBits() % 2 == 0
    }
