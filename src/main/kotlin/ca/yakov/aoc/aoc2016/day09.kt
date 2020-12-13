package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution

private typealias LengthAndIndex = Pair<Long, Int>

private enum class Stage { IDLE, LENGTH, REPEAT }

fun run09(input: String): Solution {
    val inputTrimmed = input.trim()
    return Solution(
        decompress(inputTrimmed, 0, inputTrimmed.length, false).first.toULong(),
        decompress(inputTrimmed, 0, inputTrimmed.length, true).first.toULong()
    )
}

private fun decompress(input: String, initialIndex: Int, maxLength: Int, recursive: Boolean): LengthAndIndex {
    var stage = Stage.IDLE
    var index = initialIndex
    var totalLength = 0L
    var length = 0
    var repeat = 0

    while (index < initialIndex + maxLength) {
        val here = input[index]
        when (stage) {
            Stage.IDLE -> {
                when (here) {
                    '(' -> stage = Stage.LENGTH
                    else -> totalLength++
                }
            }
            Stage.LENGTH -> {
                when (here) {
                    'x' -> stage = Stage.REPEAT
                    else -> length = length * 10 + (here - '0')
                }
            }
            Stage.REPEAT -> {
                when (here) {
                    ')' -> {
                        if (recursive) {
                            val decompressedChunk = decompress(input, index + 1, length, true)
                            totalLength += decompressedChunk.first * repeat
                            index = decompressedChunk.second - 1
                        } else {
                            totalLength += length * repeat
                            index += length
                        }
                        length = 0
                        repeat = 0
                        stage = Stage.IDLE
                    }
                    else -> repeat = repeat * 10 + (here - '0')
                }
            }
        }
        index++
    }
    return totalLength to index
}
