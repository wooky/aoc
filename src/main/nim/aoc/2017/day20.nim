import std/strscans, std/strutils, std/tables, zero_functional, ../aoc

type
  Coord = tuple[x: int, y: int, z: int]
  Particle = object
    p, v, a: Coord
    collided: bool

proc day20*(file: string): Solution =
  var particles = newSeq[Particle]()
  for line in file.splitLines():
    if line == "":
      continue
    var px, py, pz, vx, vy, vz, ax, ay, az: int
    discard line.scanf("p=<$i,$i,$i>, v=<$i,$i,$i>, a=<$i,$i,$i>", px, py, pz, vx, vy, vz, ax, ay, az)
    particles &= Particle(
      p: (px, py, pz), v: (vx, vy, vz), a: (ax, ay, az),
      collided: false,
    )

  for _ in 1 .. 1000:
    var collisions = newTable[Coord, seq[int]]()

    for i in 0 ..< particles.len():
      particles[i].v.x += particles[i].a.x
      particles[i].v.y += particles[i].a.y
      particles[i].v.z += particles[i].a.z
      particles[i].p.x += particles[i].v.x
      particles[i].p.y += particles[i].v.y
      particles[i].p.z += particles[i].v.z

      if not particles[i].collided:
        collisions.mgetOrPut(particles[i].p, newSeq[int]()) &= i
    
    collisions.mvalues -->
      filter(it.len > 1) -->
      foreach(for i in it: particles[i].collided = true)

  let closestParticle =
    particles -->
      indexedMap(it.p.x.abs() + it.p.y.abs() + it.p.z.abs()) -->
      reduce(if it.elem.elem < it.accu.elem: it.elem else: it.accu).
      idx
  echo closestParticle

  let uncollided =
    particles -->
      filter(not it.collided) -->
      count()
  echo uncollided

  Solution(s1: $closestParticle, s2: $uncollided)
