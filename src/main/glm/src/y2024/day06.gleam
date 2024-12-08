import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import lib/coord.{type Coord, type Delta, Coord, Delta}

pub fn day06(input: String) -> #(String, String) {
  let ReadState(map, guard_coord, _) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(
      ReadState(dict.new(), Coord(-1, -1), Coord(0, 0)),
      fn(read_state, c) {
        let ReadState(map, guard_coord, curr_coord) = read_state
        case string.utf_codepoint_to_int(c) {
          0x0a -> ReadState(map, guard_coord, coord.crlf(curr_coord))
          0x23 ->
            ReadState(
              dict.insert(map, curr_coord, True),
              guard_coord,
              coord.next_column(curr_coord),
            )
          0x2e ->
            ReadState(
              dict.insert(map, curr_coord, False),
              guard_coord,
              coord.next_column(curr_coord),
            )
          0x5e ->
            ReadState(
              dict.insert(map, curr_coord, False),
              curr_coord,
              coord.next_column(curr_coord),
            )
          _ -> panic
        }
      },
    )
  let guard_delta = Delta(-1, 0)

  let assert Ok(first_walk) = walk(map, guard_coord, guard_delta, set.new())
  let first_walk =
    set.fold(first_walk, set.new(), fn(acc, x) { set.insert(acc, x.0) })
  let s1 = set.size(first_walk)
  let s2 =
    first_walk
    |> set.filter(fn(coord) {
      walk(dict.insert(map, coord, True), guard_coord, guard_delta, set.new())
      |> result.is_error()
    })
    |> set.size()

  #(int.to_string(s1), int.to_string(s2))
}

type Visited =
  Set(#(Coord, Delta))

type ReadState {
  ReadState(map: Dict(Coord, Bool), guard_coord: Coord, curr_coord: Coord)
}

fn walk(
  map: Dict(Coord, Bool),
  guard_coord: Coord,
  guard_delta: Delta,
  visited: Visited,
) -> Result(Visited, Nil) {
  let value = #(guard_coord, guard_delta)
  case set.contains(visited, value) {
    True -> Error(Nil)
    False -> {
      let visited = set.insert(visited, value)
      let next_coord = coord.add(guard_coord, guard_delta)
      case dict.get(map, next_coord) {
        Ok(True) ->
          walk(map, guard_coord, coord.rotate_right(guard_delta), visited)
        Ok(False) -> walk(map, next_coord, guard_delta, visited)
        Error(_) -> Ok(visited)
      }
    }
  }
}
