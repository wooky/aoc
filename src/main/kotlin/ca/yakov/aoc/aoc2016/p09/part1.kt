package ca.yakov.aoc.aoc2016.p09

fun main(args: Array<String>) {
    var stage = Stage.IDLE
    var index = 0
    var totalLength = 0
    var length = 0
    var repeat = 0

    while(index < input.length) {
        val here = input[index]
        when(stage) {
            Stage.IDLE -> {
                when(here) {
                    '(' -> stage = Stage.LENGTH
                    else -> totalLength++
                }
            }
            Stage.LENGTH -> {
                when(here) {
                    'x' -> stage = Stage.REPEAT
                    else -> length = length*10 + (here - '0')
                }
            }
            Stage.REPEAT -> {
                when(here) {
                    ')' -> {
                        totalLength += length * repeat
                        index += length
                        length = 0
                        repeat = 0
                        stage = Stage.IDLE
                    }
                    else -> repeat = repeat*10 + (here - '0')
                }
            }
        }
        index++
    }

    print(totalLength)
}