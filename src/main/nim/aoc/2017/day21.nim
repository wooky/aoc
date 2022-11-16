import pixie, std/hashes, std/re, std/strutils, std/tables, zero_functional, ../aoc

const
  ColorOn = rgbx(255, 255, 255, 255)
  ColorOff = rgbx(0, 0, 0, 255)

proc `==`(x, y: Image): bool =
  x.width == y.width and x.height == y.height and x.data == y.data

proc hash(x: Image): Hash =
  var h: Hash = 0
  h = h !& x.width.hash()
  h = h !& x.height.hash()
  h = h !& x.data.hash()
  result = !$h

proc pixelsOn(img: Image): int =
  img.data -->
    filter(it == ColorOn) -->
    count()

proc toImage(s: seq[string]): Image =
  let image = newImage(s.len(), s.len())
  s -->
    flatten() -->
    indexedMap(if it == '#': ColorOn else: ColorOff) -->
    foreach(block: image.data[it.idx] = it.elem)
  image

proc day21*(file: string): Solution =
  var rules = newTable[Image, Image]()
  for line in file.splitLines():
    if line == "":
      continue
    var tokens: array[7, string]
    var (ruleInput, ruleOutput) =
      if line.find(re"(..)/(..) => (...)/(...)/(...)", tokens) == 0:
        (
          tokens[0 .. 1].toImage(),
          tokens[2 .. 4].toImage(),
        )
      elif line.find(re"(...)/(...)/(...) => (....)/(....)/(....)/(....)", tokens) == 0:
        (
          tokens[0 .. 2].toImage(),
          tokens[3 .. 6].toImage(),
        )
      else:
        raise newException(ValueError, "Invalid line " & line)
    
    for _ in 1 .. 4:
      rules[ruleInput] = ruleOutput
      ruleInput = ruleInput.copy()
      ruleInput.rotate90()
    ruleInput.flipHorizontal()
    for _ in 1 .. 4:
      rules[ruleInput] = ruleOutput
      ruleInput = ruleInput.copy()
      ruleInput.rotate90()

  var pixelsOn5: int
  let pixelsOn18 = block:
    var finalImage = @[".#.", "..#", "###"].toImage()
    for i in 1 .. 18:
      let (srcDim, dstDim) =
        if finalImage.width %% 2 == 0: (2, 3)
        else: (3, 4)

      var nextImage = newImage(finalImage.width /% srcDim * dstDim, finalImage.height /% srcDim * dstDim)
      for x in 0 ..< finalImage.width /% srcDim:
        for y in 0 ..< finalImage.height /% srcDim:
          let srcImage = finalImage.subImage(x * srcDim, y * srcDim, srcDim, srcDim)
          let dstImage = rules[srcImage]
          nextImage.draw(dstImage, translate(vec2(float(x * dstDim), float(y * dstDim))))
      finalImage = nextImage

      if i == 5:
        pixelsOn5 = finalImage.pixelsOn()

    finalImage.pixelsOn()

  Solution(s1: $pixelsOn5, s2: $pixelsOn18)
  