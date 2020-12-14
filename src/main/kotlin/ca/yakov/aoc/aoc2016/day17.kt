package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution
import com.soywiz.krypto.md5

private data class Event(val path: String, val x: Int, val y: Int) {
    fun up() = Event(path + "U", x, y - 1)
    fun down() = Event(path + "D", x, y + 1)
    fun left() = Event(path + "L", x - 1, y)
    fun right() = Event(path + "R", x + 1, y)
}

fun run17(inputUntrimmed: String): Solution {
    val input = inputUntrimmed.trim()
    val events = mutableListOf(Event("", 0, 0))
    var shortest = ""
    var longest = 0uL

    while (events.isNotEmpty()) {
        val event = events.removeFirst()
        if (event.x == 3 && event.y == 3) {
            if (shortest.isEmpty()) {
                shortest = event.path
            }
            longest = event.path.length.toULong()
            continue
        }

        val hash = "$input${event.path}".encodeToByteArray().md5()
        if (event.x < 3 && (hash.bytes[1].toInt() and 0xf) >= 0xb) {
            events += event.right()
        }
        if (event.y < 3 && (hash.bytes[0].toInt() and 0xf) >= 0xb) {
            events += event.down()
        }
        if (event.x > 0 && (hash.bytes[1].toInt() and 0xff) >= 0xb0) {
            events += event.left()
        }
        if (event.y > 0 && (hash.bytes[0].toInt() and 0xff) >= 0xb0) {
            events += event.up()
        }
    }

    return Solution(p1 = 0uL, s1 = shortest, p2 = longest)
}
