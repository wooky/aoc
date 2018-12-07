package ca.yakov.aoc.aoc2016.p12

fun main(args: Array<String>) {
    for (c in 0..1) {
        val registers = mutableMapOf("c" to c)
        var pc = 0
        while (pc < input.size) {
            val cmd = input[pc].split(" ")
            when (cmd[0]) {
                "cpy" -> registers[cmd[2]] = cmd[1].toIntOrNull() ?: registers[cmd[1]]!!
                "inc" -> registers[cmd[1]] = registers.getOrDefault(cmd[1], 0) + 1
                "dec" -> registers[cmd[1]] = registers.getOrDefault(cmd[1], 0) - 1
                "jnz" -> if (cmd[1].toIntOrNull() ?: registers.getOrDefault(cmd[1], 0) != 0) pc += cmd[2].toInt() - 1
                else -> throw IllegalArgumentException(cmd[0])
            }
            pc++
        }

        println(registers["a"]!!)
    }
}