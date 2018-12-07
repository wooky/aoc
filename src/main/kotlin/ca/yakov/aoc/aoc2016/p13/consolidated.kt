package ca.yakov.aoc.aoc2016.p13

import java.util.LinkedList

class Node(val coord: Coord, val dist: Int)

/**
 * Consolidated part 1 & 2 solver
 */
fun main(args: Array<String>) {
    val closedSet = mutableSetOf(start)
    val openMap = mutableMapOf<Coord, Stats>()
    val queue = LinkedList<Node>().also{ it.add(Node(Coord(1, 1), 0)) }

    while(!queue.isEmpty()) {
        val node = queue.remove()
        val coord = node.coord
        closedSet.add(coord)
        if(node.dist == 50) {
            openMap += coord to Stats(coord, 50)
            continue
        }

        listOf(Coord(coord.x-1, coord.y), Coord(coord.x+1, coord.y), Coord(coord.x, coord.y-1), Coord(coord.x, coord.y+1))
                .filter{ it !in closedSet && it.x >= 0 && it.y >= 0 && isSpace(it) }
                .forEach{
                    closedSet += it
                    queue += Node(it, node.dist+1)
                }
    }

    val visited = closedSet.size
    aStar(openMap, closedSet)
    println(visited)
}
