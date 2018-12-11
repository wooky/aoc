package ca.yakov.aoc.aoc2016.p15

fun main(args: Array<String>) {
    val discs = input.mapIndexed { n, d ->
        d.split(" ").let{ seg -> {t: Int -> (seg[11].toInt() + n+1 + t) % seg[3].toInt()} }
    }.toMutableList()
    timing(discs)

    discs += {t -> (input.size+1 + t) % 11}
    timing(discs)
}

fun timing(discs: List<(Int) -> Int>) =
        (0..Int.MAX_VALUE)
                .first { t ->
                    discs.all{ it(t) == 0 }
                }
                .also(::println)