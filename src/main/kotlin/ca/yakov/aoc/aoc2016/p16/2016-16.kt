package ca.yakov.aoc.aoc2016.p16

fun main(args: Array<String>) {
    checkMe(272)
    checkMe(35651584)
}

fun checkMe(length: Int) {
    var a = input
    while(a.length < length) {
        val b = a.reversed().map{ if(it == '0') '1' else '0' }.joinToString("")
        a += "0$b"
    }
    var checksum = a.substring(0, length)
    while(checksum.length % 2 == 0) {
        checksum = checksum.chunked(2) { if(it[0] == it[1]) 1 else 0 }.joinToString("")
    }
    println(checksum)
}