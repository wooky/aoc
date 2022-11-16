import std/strutils, ../aoc

type
  FirewallLayer = tuple[depth: int, hitTime: int]

proc day13*(file: string): Solution =
  var severity = 0
  var firewallLayers = newSeq[FirewallLayer]()
  for line in file.splitLines():
    if line == "":
      continue
    let tokens = line.split(": ")
    let firewallRange = tokens[1].parseInt()
    let firewallLayer: FirewallLayer = (
      depth: tokens[0].parseInt(),
      hitTime: (firewallRange - 1) * 2,
    )
    firewallLayers &= firewallLayer
    if firewallLayer.depth %% firewallLayer.hitTime == 0:
      severity += firewallLayer.depth * firewallRange

  var delay = 0
  block outer:
    while true:
      block inner:
        for firewallLayer in firewallLayers:
          if (firewallLayer.depth + delay) %% firewallLayer.hitTime == 0:
            break inner
        break outer
      delay += 1

  Solution(s1: $severity, s2: $delay)
