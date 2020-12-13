package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

private class Room(room: String) {
    val name: String
    val sectorId: Int
    val checksum: String

    init {
        val seq = room.split("-")
        this.name = seq.subList(0, seq.size - 1).joinToString("")
        val suf = seq.last().split("[", "]")
        this.sectorId = suf[0].toInt()
        this.checksum = suf[1]
    }
}

fun run04(input: String): Solution {
    val realRooms =
        input
            .trim()
            .lineSequence()
            .map { Room(it) }
            .filter { room ->
                room
                    .name
                    .groupingBy { it }
                    .eachCount()
                    .toList()
                    .sortedWith(compareByDescending<Pair<Char, Int>> { it.second }.thenBy { it.first })
                    .subList(0, 5)
                    .joinToString("", transform = { it.first.toString() }) == room.checksum
            }
    val res1 = realRooms.sumBy { it.sectorId }.toULong()

    val res2 =
        realRooms
            .first { room ->
                room
                    .name
                    .map { it + (room.sectorId % 26) }
                    .map { if (it > 'z') it - 26 else it }
                    .joinToString("") == "northpoleobjectstorage"
            }
            .sectorId
            .toULong()

    return Solution(res1, res2)
}
