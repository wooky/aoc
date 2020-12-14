package ca.yakov.aoc.aoc2016

import ca.yakov.aoc.Solution
import com.soywiz.krypto.md5

fun run14(untrimmedInput: String): Solution {
    val input = untrimmedInput.trim()
    return Solution(
        oneTimePad(input, 1),
        oneTimePad(input, 2017)
    )
}

// TODO parallelize this!
private fun oneTimePad(input: String, repeat: Int): ULong {
    val triple = Regex("(.)\\1\\1")
    val seqs = mutableListOf<Pair<Int, String>>()
    var keysGenerated = 0
    for(i in 0..Int.MAX_VALUE) {
        seqs.removeAll { it.first == i - 1001 }
        var key = "$input$i"
        for (j in 1..repeat) {
            key = key.encodeToByteArray().md5().hex
        }
        val seq = triple.find(key)
        if(seq != null) {
            val iter = seqs.iterator()
            for((k, v) in iter) {
                if(v in key) {
                    if(keysGenerated == 63) {
                        return k.toULong()
                    }
                    iter.remove()
                    keysGenerated++
                }
            }
            seqs += i to "${seq.value[0]}".repeat(5)
        }
    }
    throw IllegalStateException()
}
