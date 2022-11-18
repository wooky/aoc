import ../aoc

proc checkMe(input: string, length: int): string =
  var a = input
  while a.len() < length:
    a &= '0'
    for i in countdown(a.len() - 2, 0):
      a &= (if a[i] == '0': '1' else: '0')
  
  var checksum = a[0 ..< length]
  while checksum.len() %% 2 == 0:
    var newChecksum = ""
    for i in countup(0, checksum.len() - 1, 2):
      newChecksum &= (if checksum[i] == checksum[i + 1]: '1' else: '0')
    checksum = newChecksum

  checksum

proc day16*(file: string): Solution =
  Solution(
    s1: file.checkMe(272),
    s2: file.checkMe(35651584),
  )
