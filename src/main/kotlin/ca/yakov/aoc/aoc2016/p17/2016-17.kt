package ca.yakov.aoc.aoc2016.p17

import java.security.MessageDigest
import java.util.LinkedList

data class Event(val path: String, val x: Int, val y: Int) {
    fun up() = Event(path + "U", x, y-1)
    fun down() = Event(path + "D", x, y+1)
    fun left() = Event(path + "L", x-1, y)
    fun right() = Event(path + "R", x+1, y)
}

fun main(args: Array<String>) {
    val md5 = MessageDigest.getInstance("MD5")
    val events = LinkedList<Event>().also{ it += Event("", 0, 0) }
    var gotShortest = false
    var longest = 0

    while(!events.isEmpty()) {
        val event = events.remove()
        if(event.x == 3 && event.y == 3) {
            if(!gotShortest) {
                println(event.path)
                gotShortest = true
            }
            longest = event.path.length
            continue
        }

        val hash = md5.digest("$input${event.path}".toByteArray())
        if(event.x < 3 && (hash[1].toInt() and 0xf) >= 0xb) {
            events += event.right()
        }
        if(event.y < 3 && (hash[0].toInt() and 0xf) >= 0xb) {
            events += event.down()
        }
        if(event.x > 0 && (hash[1].toInt() and 0xff) >= 0xb0) {
            events += event.left()
        }
        if(event.y > 0 && (hash[0].toInt() and 0xff) >= 0xb0) {
            events += event.up()
        }
    }

    println(longest)
}