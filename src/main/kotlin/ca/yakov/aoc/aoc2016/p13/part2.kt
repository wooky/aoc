package ca.yakov.aoc.aoc2016.p13

import java.util.LinkedList

data class Coord(val x: Int, val y: Int)
class Node(val coord: Coord, val dist: Int)
val spaces = mutableMapOf<Coord, Boolean>()

fun main(args: Array<String>) {
    val visited = mutableSetOf(Coord(1, 1))
    val queue = LinkedList<Node>().also{ it.add(Node(Coord(1, 1), 0)) }

    while(!queue.isEmpty()) {
        val node = queue.remove()
        val coord = node.coord
        visited.add(coord)
        if(node.dist == 50) {
            continue
        }

        listOf(Coord(coord.x-1, coord.y), Coord(coord.x+1, coord.y), Coord(coord.x, coord.y-1), Coord(coord.x, coord.y+1))
                .filter{ it !in visited && it.x >= 0 && it.y >= 0 && isSpace(it) }
                .forEach{
                    visited += it
                    queue += Node(it, node.dist+1)
                }
    }

    println(visited.size)
}

fun isSpace(coord: Coord) =
        spaces.getOrPut(coord) {
            Integer
                    .toBinaryString(coord.x*coord.x + 3*coord.x + 2*coord.x*coord.y + coord.y + coord.y*coord.y + input)
                    .count{ it == '1' } % 2 == 0
        }