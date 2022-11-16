import zero_functional, ../aoc

proc captcha(file: string, cutoff: int): int =
  zip(file, (file[cutoff .. ^1] & file[0 ..< cutoff])) -->
    filter(it[0] == it[1]) -->
    map(int(it[0]) - int('0')) -->
    sum()

proc day01*(file: string): Solution =
  Solution(
    s1: $captcha(file, 1),
    s2: $captcha(file, file.len /% 2),
  )
