import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string
import lib/coord.{type Coord, type Delta, Coord}

pub fn day08(input: String) -> #(String, String) {
  let #(antennas, max_coord) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), Coord(0, 0)), fn(acc, codepoint) {
      let #(antennas, coord) = acc
      case string.utf_codepoint_to_int(codepoint) {
        0x0a -> #(antennas, coord.crlf(coord))
        0x2e -> #(antennas, coord.next_column(coord))
        char -> {
          let antennas =
            dict.upsert(antennas, char, fn(x) {
              case x {
                Some(coords) -> [coord, ..coords]
                None -> [coord]
              }
            })
          #(antennas, coord.next_column(coord))
        }
      }
    })
  let #(s1, s2) =
    antennas
    |> dict.values()
    |> list.flat_map(fn(coords) { list.combination_pairs(coords) })
    |> list.fold(#(set.new(), set.new()), fn(acc, ants) {
      let #(s1, s2) = acc
      let #(coord1, coord2) = ants
      let delta = coord.diff(coord2, coord1)
      let neg_delta = coord.negate(delta)
      let s1 =
        s1
        |> insert_antinodes(coord1, neg_delta, max_coord, 1)
        |> insert_antinodes(coord2, delta, max_coord, 1)
      let s2 =
        s2
        |> set.insert(coord1)
        |> set.insert(coord2)
        |> insert_antinodes(coord1, neg_delta, max_coord, -1)
        |> insert_antinodes(coord2, delta, max_coord, -1)
      #(s1, s2)
    })

  #(int.to_string(set.size(s1)), int.to_string(set.size(s2)))
}

fn insert_antinodes(
  ants: Set(Coord),
  coord: Coord,
  delta: Delta,
  max_coord: Coord,
  limit: Int,
) -> Set(Coord) {
  let coord = coord.add(coord, delta)
  // Max column is 1 more than the real max!
  let out_of_bounds =
    coord.row < 0
    || coord.col < 0
    || coord.row > max_coord.row
    || coord.col >= max_coord.col
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
