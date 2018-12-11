package ca.yakov.aoc.aoc2016.p14

import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

/**
 * TODO there's a bug in the program where if the generated keys are not retrieved in order.
 * To fix it, each triple matched seq needs to know if the next 1,000 hashes are valid, not the other way around
 */
fun main(args: Array<String>) {
    val md5 = MessageDigest.getInstance("MD5")
    val triple = Regex("(.)\\1\\1")
    val seqs = sortedMapOf<Int, String>()
    var keysGenerated = 0
    for(i in 0..Int.MAX_VALUE) {
        seqs -= i-1001
        var key = "$input$i"
        for(j in 1..2017) {
            key = DatatypeConverter.printHexBinary(md5.digest(key.toByteArray())).toLowerCase()
        }
        val seq = triple.find(key)
        if(seq != null) {
            val iter = seqs.iterator()
            for((k, v) in iter) {
                if(v in key) {
                    if(keysGenerated == 63) {
                        println(k)
                        return
                    }
                    iter.remove()
                    keysGenerated++
                }
            }
            seqs += i to Character.toString(seq.value[0]).repeat(5)
        }
    }
}