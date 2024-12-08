import gleam/yielder.{type Yielder}

pub type Coord {
  Coord(row: Int, col: Int)
}

pub type Delta {
  Delta(drow: Int, dcol: Int)
}

pub fn neighbor_deltas() -> List(Delta) {
  [
    Delta(-1, -1),
    Delta(-1, 0),
    Delta(-1, 1),
    Delta(0, -1),
    Delta(0, 1),
    Delta(1, -1),
    Delta(1, 0),
    Delta(1, 1),
  ]
}

pub fn add(this: Coord, delta: Delta) -> Coord {
  Coord(this.row + delta.drow, this.col + delta.dcol)
}

pub fn sub(this: Coord, delta: Delta) -> Coord {
  Coord(this.row - delta.drow, this.col - delta.dcol)
}

pub fn diff(from: Coord, to: Coord) -> Delta {
  Delta(from.row - to.row, from.col - to.col)
}

pub fn neighbors(this: Coord) -> Yielder(Coord) {
  neighbor_deltas()
  |> yielder.from_list()
  |> yielder.map(fn(x) { add(this, x) })
}

pub fn next_column(this: Coord) -> Coord {
  Coord(this.row, this.col + 1)
}

pub fn crlf(this: Coord) -> Coord {
  Coord(this.row + 1, 0)
}

pub fn rotate_right(this: Delta) -> Delta {
  Delta(this.dcol, -this.drow)
}

pub fn negate(this: Delta) -> Delta {
  Delta(-this.drow, -this.dcol)
}
