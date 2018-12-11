package ca.yakov.aoc.aoc2016.p14

import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

fun main(args: Array<String>) {
    val md5 = MessageDigest.getInstance("MD5")
    val triple = Regex("(.)\\1\\1")
    val seqs = sortedMapOf<Int, String>()
    var keysGenerated = 0
    for(i in 0..Int.MAX_VALUE) {
        seqs -= i-1001
        val key = DatatypeConverter.printHexBinary(md5.digest("$input$i".toByteArray()))
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