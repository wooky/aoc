package ca.yakov.aoc.aoc2016.p07

class Ip(ip: String) {
    val outsides: List<String>
    val insides: List<String>

    init {
        val seqs = ip.split("[", "]")
        outsides = seqs.filterIndexed{ i, _ -> i % 2 == 0 }
        insides = seqs.filterIndexed{ i, _ -> i % 2 == 1 }
    }
}

fun main(args: Array<String>) {
    val ips = input.map{ Ip(it) }
    println(ips.filter{ it.insides.none(::isAbba) && it.outsides.any(::isAbba) }.count())
    ips
            .filter {
                val babs = it.outsides.map(::getBabsFromAbas).flatten()
                it.insides.any{ inside -> babs.any{ inside.contains(it) } }
            }
            .count()
            .also(::println)
}

fun isAbba(seg: String) =
        seg
                .windowed(4)
                .any{ it[0] != it[1] && it[0] == it[3] && it[1] == it[2] }

fun getBabsFromAbas(seg: String) =
        seg
                .windowed(3)
                .filter{ it[0] != it[1] && it[0] == it[2] }
                .map{ "${it[1]}${it[0]}${it[1]}" }
