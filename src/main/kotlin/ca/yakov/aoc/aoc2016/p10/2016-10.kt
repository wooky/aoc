package ca.yakov.aoc.aoc2016.p10

import java.util.*

sealed class Depot
class OtherBot(val id: String) : Depot()
class Output(val id: String) : Depot()

class Bot(val id: String) {
    val chips = TreeSet<Int>()
    lateinit var low: Depot
    lateinit var high: Depot
}

fun main(args: Array<String>) {
    val bots = mutableMapOf<String, Bot>()
    val queue = LinkedList<Bot>()
    val outputs = mutableMapOf<String, Int>()
    input.forEach {
        val cmd = it.split(" ")
        when(cmd[0]) {
            "bot" -> {
                val id = cmd[1]
                val bot = bots.getOrPut(id, {Bot(id)})
                bot.low = if(cmd[5] == "output") Output(cmd[6]) else OtherBot(cmd[6])
                bot.high = if(cmd[10] == "output") Output(cmd[11]) else OtherBot(cmd[11])
            }
            "value" -> {
                val id = cmd[5]
                val bot = bots.getOrPut(id, {Bot(id)})
                bot.chips += cmd[1].toInt()
                if(bot.chips.size == 2) {
                    queue += bot
                }
            }
            else -> throw IllegalArgumentException()
        }
    }

    val expected = sortedSetOf(17, 61)
    while(!queue.isEmpty()) {
        val bot = queue.remove()
        if(bot.chips == expected) {
            println(bot.id)
        }

        deposit(bot.low, bot.chips.first(), bots, queue, outputs)
        deposit(bot.high, bot.chips.last(), bots, queue, outputs)
        bot.chips.clear()
    }

    println(outputs["0"]!! * outputs["1"]!! * outputs["2"]!!)
}

fun deposit(depot: Depot, chip: Int, bots: Map<String, Bot>, queue: LinkedList<Bot>, outputs: MutableMap<String, Int>) {
    when(depot) {
        is OtherBot -> {
            val bot = bots[depot.id]!!
            bot.chips += chip
            if(bot.chips.size == 2) {
                queue += bot
            }
        }
        is Output -> outputs[depot.id] = chip
    }
}