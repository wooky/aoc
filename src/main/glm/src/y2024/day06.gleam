import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/yielder

pub fn day06(input: String) -> #(String, String) {
  let ReadState(map, guard_coord, max_row, max_col) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(ReadState(dict.new(), #(-1, -1), 0, 0), fn(read_state, c) {
      let ReadState(map, guard_coord, row, col) = read_state
      case string.utf_codepoint_to_int(c) {
        0x0a -> ReadState(map, guard_coord, row + 1, 0)
        0x23 ->
          ReadState(
            dict.insert(map, #(row, col), True),
            guard_coord,
            row,
            col + 1,
          )
        0x2e ->
          ReadState(
            dict.insert(map, #(row, col), False),
            guard_coord,
            row,
            col + 1,
          )
        0x5e ->
          ReadState(
            dict.insert(map, #(row, col), False),
            #(row, col),
            row,
            col + 1,
          )
        _ -> panic
      }
    })
  let guard_delta = #(-1, 0)

  let assert Ok(s1_walk) = walk(map, guard_coord, guard_delta, set.new())
  let s1 =
    s1_walk
    |> set.fold(set.new(), fn(acc, x) { set.insert(acc, x.0) })
    |> set.size()

  let walk_results = {
    use row <- yielder.flat_map(yielder.range(0, max_row))
    use col <- yielder.map(yielder.range(0, max_col))
    walk(
      dict.insert(map, #(row, col), True),
      guard_coord,
      guard_delta,
      set.new(),
    )
  }
  let s2 =
    walk_results
    |> yielder.filter(fn(x) { result.is_error(x) })
    |> yielder.length()

  #(int.to_string(s1), int.to_string(s2))
}

type Coord =
  #(Int, Int)

type Visited =
  Set(#(Coord, Coord))

type ReadState {
  ReadState(map: Dict(Coord, Bool), guard_coord: Coord, row: Int, col: Int)
}

fn walk(
  map: Dict(Coord, Bool),
  guard_coord: Coord,
  guard_delta: Coord,
  visited: Visited,
) -> Result(Visited, Nil) {
  let value = #(guard_coord, guard_delta)
  case set.contains(visited, value) {
    True -> Error(Nil)
    False -> {
      let visited = set.insert(visited, value)
      let next_coord = #(
        guard_coord.0 + guard_delta.0,
        guard_coord.1 + guard_delta.1,
      )
      case dict.get(map, next_coord) {
        Ok(True) ->
          walk(map, guard_coord, #(guard_delta.1, -guard_delta.0), visited)
        Ok(False) -> walk(map, next_coord, guard_delta, visited)
        Error(_) -> Ok(visited)
      }
    }
  }
}
