import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/set
import gleam/string
import gleam/yielder
import lib/coord.{type Coord, Coord}

pub fn day10(input: String) -> #(String, String) {
  let #(topography, _) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), Coord(0, 0)), fn(acc, token) {
      let #(topography, coord) = acc
      case string.utf_codepoint_to_int(token) {
        0x0a -> #(topography, coord.crlf(coord))
        c -> #(
          dict.insert(topography, coord, c - 0x30),
          coord.next_column(coord),
        )
      }
    })
  let peaks =
    topography
    |> dict.filter(fn(_, height) { height == 0 })
    |> dict.keys()
    |> list.map(fn(coord) { climb(topography, coord, 0) })
  let s1 =
    list.fold(peaks, 0, fn(acc, peaks) {
      acc + { peaks |> set.from_list() |> set.size() }
    })
  let s2 = peaks |> list.flatten() |> list.length()

  #(int.to_string(s1), int.to_string(s2))
}

fn climb(topography: Dict(Coord, Int), coord: Coord, target: Int) -> List(Coord) {
  case dict.get(topography, coord), target {
    Ok(x), 9 if x == target -> [coord]
    Ok(x), _ if x == target ->
      coord
      |> coord.manhattan()
      |> yielder.fold([], fn(peaks, coord) {
        list.append(peaks, climb(topography, coord, target + 1))
      })
    _, _ -> []
  }
}
