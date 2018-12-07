package ca.yakov.aoc.aoc2016.p09

fun main(args: Array<String>) {
    println(decompress(0, input.length).first)
}

fun decompress(initialIndex: Int, maxLength: Int): Pair<Long, Int> {
    var stage = Stage.IDLE
    var index = initialIndex
    var totalLength = 0L
    var length = 0
    var repeat = 0

    while(index < initialIndex + maxLength) {
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
                        val decompressedChunk = decompress(index+1, length)
                        totalLength += decompressedChunk.first * repeat
                        index = decompressedChunk.second - 1
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
    return totalLength to index
}