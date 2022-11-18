import zero_functional, ../aoc

proc countSafe(row: seq[char]): int =
  row --> map(if it == '.': 1 else: 0) --> sum()

proc isTrap(subrow: seq[char]): bool =
  (subrow[0] == '^' and subrow[1] == '^' and subrow[2] == '.') or
  (subrow[0] == '.' and subrow[1] == '^' and subrow[2] == '^') or
  (subrow[0] == '^' and subrow[1] == '.' and subrow[2] == '.') or
  (subrow[0] == '.' and subrow[1] == '.' and subrow[2] == '^')

proc day18*(file: string): Solution =
  var row = cast[seq[char]](file)
  var safe = row.countSafe()
  var safe40 = 0
  for i in 2 .. 400000:
    row.insert('.', 0)
    row &= '.'
    row =
      (0 .. row.len() - 3) -->
        map(row[it ..< it + 3]) -->
        map(if it.isTrap(): '^' else: '.')
    safe += row.countSafe()
    if i == 40:
      safe40 = safe

  return Solution(s1: $safe40, s2: $safe)
