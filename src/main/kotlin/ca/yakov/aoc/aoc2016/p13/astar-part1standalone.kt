package ca.yakov.aoc.aoc2016.p13

/**
 * Standalone part 1 solver
 */
fun main(args: Array<String>) {
    aStar(mutableMapOf(start to Stats(start, 0)), mutableSetOf())
}

fun aStar(openMap: MutableMap<Coord, Stats>, closedSet: MutableSet<Coord>) {
    while (true) {
        val node = openMap.minBy { (_, v) -> v.fScore }!!
        val coord = node.key
        if (coord == goal) {
            println(node.value.gScore)
            return
        }

        openMap -= coord
        closedSet += coord
        val tentativeGscore = node.value.gScore + 1

        listOf(Coord(coord.x-1, coord.y), Coord(coord.x+1, coord.y), Coord(coord.x, coord.y-1), Coord(coord.x, coord.y+1))
                .filter{
                    it !in closedSet && it.x >= 0 && it.y >= 0 &&
                            openMap[it]?.let { it.gScore > tentativeGscore } != false &&
                            isSpace(it)
                }
                .forEach{
                    openMap += it to Stats(it, tentativeGscore)
                }
    }
}
