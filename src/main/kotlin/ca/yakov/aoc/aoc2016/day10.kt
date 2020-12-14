package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

private sealed class Depot
private class OtherBot(val id: String) : Depot()
private class Output(val id: String) : Depot()

private class Bot(val id: String) {
    val chips = mutableSetOf<Int>()
    lateinit var low: Depot
    lateinit var high: Depot
}

fun run10(input: String): Solution {
    val bots = mutableMapOf<String, Bot>()
    val queue = mutableListOf<Bot>()
    val outputs = mutableMapOf<String, Int>()
    input.trim().lineSequence().forEach {
        val cmd = it.split(" ")
        when (cmd[0]) {
            "bot" -> {
                val id = cmd[1]
                val bot = bots.getOrPut(id, { Bot(id) })
                bot.low = if (cmd[5] == "output") Output(cmd[6]) else OtherBot(cmd[6])
                bot.high = if (cmd[10] == "output") Output(cmd[11]) else OtherBot(cmd[11])
            }
            "value" -> {
                val id = cmd[5]
                val bot = bots.getOrPut(id, { Bot(id) })
                bot.chips += cmd[1].toInt()
                if (bot.chips.size == 2) {
                    queue += bot
                }
            }
            else -> throw IllegalArgumentException()
        }
    }

    val expected = setOf(17, 61)
    var expectedBotId = 0uL
    while (queue.isNotEmpty()) {
        val bot = queue.removeLast()
        if (bot.chips == expected) {
            expectedBotId = bot.id.toULong()
        }

        deposit(bot.low, bot.chips.minOrNull()!!, bots, queue, outputs)
        deposit(bot.high, bot.chips.maxOrNull()!!, bots, queue, outputs)
        bot.chips.clear()
    }

    return Solution(
        expectedBotId,
        (outputs["0"]!! * outputs["1"]!! * outputs["2"]!!).toULong()
    )
}

private fun deposit(
    depot: Depot,
    chip: Int,
    bots: Map<String, Bot>,
    queue: MutableList<Bot>,
    outputs: MutableMap<String, Int>
) {
    when (depot) {
        is OtherBot -> {
            val bot = bots[depot.id]!!
            bot.chips += chip
            if (bot.chips.size == 2) {
                queue += bot
            }
        }
        is Output -> outputs[depot.id] = chip
    }
}
