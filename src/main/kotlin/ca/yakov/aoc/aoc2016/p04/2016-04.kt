package ca.yakov.aoc.aoc2016.p04

class Room(room: String) {
    val name: String
    val sectorId: Int
    val checksum: String

    init {
        val seq = room.split("-")
        this.name = seq.subList(0, seq.size-1).joinToString("")
        val suf = seq.last().split("[", "]")
        this.sectorId = suf[0].toInt()
        this.checksum = suf[1]
    }
}

fun main(args: Array<String>) {
    val realRooms =
            input
                    .map{ Room(it) }
                    .filter {
                        it
                                .name
                                .groupingBy{ it }
                                .eachCount()
                                .toList()
                                .sortedWith( compareByDescending<Pair<Char, Int>>{ it.second }.thenBy { it.first } )
                                .subList(0, 5)
                                .joinToString("", transform = { it.first.toString() }) == it.checksum
                    }

    println(realRooms.sumBy{ it.sectorId })
    realRooms
            .first { room ->
                room
                        .name
                        .map { it + (room.sectorId % 26) }
                        .map { if(it > 'z') it-26 else it }
                        .joinToString("") == "northpoleobjectstorage"
            }
            .let{ println(it.sectorId) }
}