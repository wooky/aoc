import std/re, std/strutils, zero_functional, ../aoc

proc isAbba(s: string): bool =
  for i in 0 .. s.len() - 4:
    if s[i] != s[i + 1] and s[i] == s[i + 3] and s[i + 1] == s[i + 2]:
      return true
  false

proc babs(s: string): seq[string] =
  (0 .. s.len() - 3) -->
    filter(s[it] != s[it + 1] and s[it] == s[it + 2]) -->
    map(s[it + 1] & s[it] & s[it + 1])

proc day07*(file: string): Solution =
  var tls = 0
  var ssl = 0
  for line in file.splitLines():
    if line == "":
      continue
    var outters, inners = newSeq[string]()
    var tokens: array[1, string]
    var inOutter = true
    var findIdx = 0
    while true:
      findIdx = line.find(re"(\w+)[\[\]]?", tokens, findIdx)
      if findIdx == -1:
        break
      if inOutter:
        outters &= tokens[0]
      else:
        inners &= tokens[0]
      inOutter = not inOutter
      findIdx += tokens[0].len

    let outterAbba = outters --> exists(it.isAbba())
    let innerAbba = inners --> exists(it.isAbba())
    if outterAbba and not innerAbba:
      tls += 1

    let hasBabs =
      outters -->
        map(it.babs()) -->
        flatten() -->
        map(bab = it) -->
        exists(inners --> exists(it.find(bab) != -1))
    if hasBabs:
      ssl += 1
  
  Solution(s1: $tls, s2: $ssl)
