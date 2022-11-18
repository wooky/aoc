import std/math, std/strutils, ../aoc

proc day19*(file: string): Solution =
  let input = file.parseInt()

  # PART 1 ALGORITHM
  # Result resets to 1 whenever input is 2^x + 1
  # Result increments by 2
  let lower1 = pow(2.0, floor(log2(input.toFloat()))).toInt()
  let res1 = (input - lower1) * 2 + 1

  # PART 2 ALGORITHM
  # Result resets to 1 whenever input is 3^x + 1
  # Assume input lies in the range (3^x + 1)..3^(x+1), the range having n numbers
  # For the first n/2 numbers, the result increments by 1
  # For the last n/2 numbers, the result increments by 2

  let power2 = floor(log2(input.toFloat()) / log2(3.0))
  let lower2 = pow(3.0, power2).toInt()
  let upper2 = pow(3.0, power2 + 1).toInt()
  let mid2 = (upper2 + lower2) /% 2
  let res2 =
    if input <= mid2:
      input - lower2
    else:
      (mid2 /% 2) + (input - mid2) * 2

  Solution(s1: $res1, s2: $res2)
