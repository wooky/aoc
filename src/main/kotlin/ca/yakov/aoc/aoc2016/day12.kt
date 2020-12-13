package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

fun run12(input: String): Solution {
    val lines = input.trim().lines()
    return Solution(
        executeInstructions(lines, 0),
        executeInstructions(lines, 1)
    )
}

private fun executeInstructions(input: List<String>, c: Int): ULong {
    val registers = mutableMapOf("c" to c)
    var pc = 0
    while (pc < input.size) {
        val cmd = input[pc].split(" ")
        when (cmd[0]) {
            "cpy" -> registers[cmd[2]] = cmd[1].toIntOrNull() ?: registers[cmd[1]]!!
            "inc" -> registers[cmd[1]] = registers.getOrElse(cmd[1], { 0 }) + 1
            "dec" -> registers[cmd[1]] = registers.getOrElse(cmd[1], { 0 }) - 1
            "jnz" -> if (cmd[1].toIntOrNull() ?: registers.getOrElse(cmd[1], { 0 }) != 0) pc += cmd[2].toInt() - 1
            else -> throw IllegalArgumentException(cmd[0])
        }
        pc++
    }

    return registers["a"]!!.toULong()
}