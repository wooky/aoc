import std/strutils, zero_functional

type
  Chain = array[256, int]
  KnotHasher = ref object of RootObj
    chain*: Chain
    idx: int
    skip: int

proc newKnotHasher*(): KnotHasher =
  var chain: Chain
  for i in 0 ..< chain.len(): chain[i] = i
  KnotHasher(
    chain: chain,
    idx: 0,
    skip: 0
  )

proc feed*(hasher: KnotHasher, size: int) =
  var idxLeft = hasher.idx
  var idxRight = (hasher.idx + size - 1) %% hasher.chain.len()
  for _ in 0 ..< size /% 2:
    swap(hasher.chain[idxLeft], hasher.chain[idxRight])
    idxLeft = (idxLeft + 1) %% hasher.chain.len()
    idxRight =
      if idxRight == 0: hasher.chain.len() - 1
      else: idxRight - 1
  hasher.idx = (hasher.idx + size + hasher.skip) %% hasher.chain.len()
  hasher.skip += 1

proc sparseHash*(input: string): string =
  var hasher = newKnotHasher()
  for _ in 0 ..< 64:
    for c in input:
      hasher.feed(int(c))
    for i in [17, 31, 73, 47, 23]:
      hasher.feed(i)

  var hash = ""
  for i in countup(0, hasher.chain.len() - 1, 16):
    hash &= (hasher.chain[i ..< i + 16] --> fold(0, a xor it)).toHex(2)
  hash
