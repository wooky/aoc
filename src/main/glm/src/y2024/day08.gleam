import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string

pub fn day08(input: String) -> #(String, String) {
  let #(antennas, max_coord) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), #(0, 0)), fn(acc, codepoint) {
      let #(antennas, #(row, col) as coord) = acc
      case string.utf_codepoint_to_int(codepoint) {
        0x0a -> #(antennas, #(row + 1, 0))
        0x2e -> #(antennas, #(row, col + 1))
        char -> {
          let antennas =
            dict.upsert(antennas, char, fn(x) {
              case x {
                Some(coords) -> [coord, ..coords]
                None -> [coord]
              }
            })
          #(antennas, #(row, col + 1))
        }
      }
    })
  let #(s1, s2) =
    antennas
    |> dict.values()
    |> list.flat_map(fn(coords) { list.combination_pairs(coords) })
    |> list.fold(#(set.new(), set.new()), fn(acc, ants) {
      let #(s1, s2) = acc
      let #(#(row1, col1) as coord1, #(row2, col2) as coord2) = ants
      let drow = row2 - row1
      let dcol = col2 - col1
      let s1 =
        s1
        |> insert_antinodes(coord1, #(-drow, -dcol), max_coord, 1)
        |> insert_antinodes(coord2, #(drow, dcol), max_coord, 1)
      let s2 =
        s2
        |> set.insert(coord1)
        |> set.insert(coord2)
        |> insert_antinodes(coord1, #(-drow, -dcol), max_coord, -1)
        |> insert_antinodes(coord2, #(drow, dcol), max_coord, -1)
      #(s1, s2)
    })

  #(int.to_string(set.size(s1)), int.to_string(set.size(s2)))
}

type Coord =
  #(Int, Int)

fn insert_antinodes(
  ants: Set(Coord),
  coord: Coord,
  delta: Coord,
  max_coord: Coord,
  limit: Int,
) -> Set(Coord) {
  let coord = #(coord.0 + delta.0, coord.1 + delta.1)
  // Max column is 1 more than the real max!
  let out_of_bounds =
    coord.0 < 0
    || coord.1 < 0
    || coord.0 > max_coord.0
    || coord.1 >= max_coord.1
  case out_of_bounds, limit {
    True, _ | _, 0 -> ants
    _, _ ->
      insert_antinodes(
        set.insert(ants, coord),
        coord,
        delta,
        max_coord,
        limit - 1,
      )
  }
}
