import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import gleam/yielder
import lib/coord.{type Coord, Coord}

pub fn day12(input: String) -> #(String, String) {
  let #(map, _) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), Coord(0, 0)), fn(acc, codepoint) {
      let #(map, coord) = acc
      case string.utf_codepoint_to_int(codepoint) {
        0x0a -> #(map, coord.crlf(coord))
        c -> #(dict.insert(map, coord, c), coord.next_column(coord))
      }
    })
  let s1 = calc_cost(map, map)

  #(int.to_string(s1), "TODO")
}

type Map =
  Dict(Coord, Int)

type Region {
  Region(area: Int, perimeter: Int, rest: Map)
}

fn calc_cost(map: Map, rest: Map) -> Int {
  case dict.to_list(rest) {
    [] -> 0
    [#(coord, target), ..] -> {
      let Region(area, perimeter, rest) =
        scan_region(map, target, coord, Region(0, 0, rest))
      area * perimeter + calc_cost(map, rest)
    }
  }
}

fn scan_region(map: Map, target: Int, coord: Coord, region: Region) -> Region {
  let region =
    Region(region.area + 1, region.perimeter, dict.drop(region.rest, [coord]))
  coord
  |> coord.manhattan()
  |> yielder.fold(region, fn(region, coord) {
    case dict.get(region.rest, coord), dict.get(map, coord) {
      Ok(t), _ if t == target -> scan_region(map, target, coord, region)
      _, Ok(t) if t == target -> region
      _, _ -> Region(..region, perimeter: region.perimeter + 1)
    }
  })
}
