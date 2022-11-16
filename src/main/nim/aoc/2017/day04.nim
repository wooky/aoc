import std/sequtils, std/strutils, std/tables, zero_functional, ../aoc

proc day04*(file: string): Solution =
  let words =
    file.splitLines() -->
      filter(it != "") -->
      map(it.splitWhitespace()) -->
      to(seq)

  let validUnique =
    words -->
      filter(it.len() == it.deduplicate().len()) -->
      count()

  let validNonAnagrams =
    words -->
      map(row = it) -->
      map(
        row -->
          map(word = it) -->
          map(word --> fold(newTable[char, int](), block:
            if it in a: a[it] += 1 else: a[it] = 1
            a
          ))
      ) -->
      filter(it.len() == it.deduplicate().len()) -->
      count()

  Solution(s1: $validUnique, s2: $validNonAnagrams)
