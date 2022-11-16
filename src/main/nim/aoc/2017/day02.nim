import strutils, zero_functional, ../aoc

proc day02*(file: string): Solution =
  let numbers =
    file.splitLines() -->
      filter(it != "") -->
      map(it.splitWhitespace() --> map(it.parseInt())) -->
      to(seq)

  let checksum =
    numbers -->
      map((it.max()) - (it.min())) -->
      sum()

  let evens =
    numbers -->
      map(row = it) -->
      map(
        row -->
        combinations(row) -->
        filter(it[0] != it[1] and it[0] %% it[1] == 0) -->
        map(it[0] /% it[1])[0]
      ) -->
      sum()
  
  Solution(s1: $checksum, s2: $evens)
