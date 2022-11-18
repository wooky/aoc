import std/md5, ../aoc

type
  Event = object
    path: string
    x: int
    y: int

proc up(event: Event): Event = Event(path: event.path & 'U', x: event.x, y: event.y - 1)
proc down(event: Event): Event = Event(path: event.path & 'D', x: event.x, y: event.y + 1)
proc left(event: Event): Event = Event(path: event.path & 'L', x: event.x - 1, y: event.y)
proc right(event: Event): Event = Event(path: event.path & 'R', x: event.x + 1, y: event.y)

proc isDoorOpen(c: char): bool = int(c) >= int('b')

proc day17*(file: string): Solution =
  var events = @[Event(path: "", x: 0, y: 0)]
  var shortest = ""
  var longest = 0

  while events.len() != 0:
    let event = events[0]
    events.delete(0)
    if event.x == 3 and event.y == 3:
      if shortest.len() == 0:
        shortest = event.path
      longest = event.path.len()
      continue

    let hash = (file & event.path).getMD5()
    if event.y > 0 and hash[0].isDoorOpen(): events &= event.up()
    if event.y < 3 and hash[1].isDoorOpen(): events &= event.down()
    if event.x > 0 and hash[2].isDoorOpen(): events &= event.left()
    if event.x < 3 and hash[3].isDoorOpen(): events &= event.right()

  Solution(s1: shortest, s2: $longest)
