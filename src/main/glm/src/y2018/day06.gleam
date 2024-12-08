import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string
import gleam/yielder
import lib/coord.{type Coord, Coord}

pub fn day06(input: String) -> #(String, String) {
  let assert #(areas, BoundingBox(from, to)) =
    input
    |> string.split("\n")
    |> list.fold(#([], NoBox), fn(acc, line) {
      let #(areas, bounding_box) = acc
      let assert [Ok(col), Ok(row)] =
        line |> string.split(", ") |> list.map(int.parse)
      let coord = Coord(row, col)
      let bounding_box = case bounding_box {
        BoundingBox(from, to) ->
          BoundingBox(
            Coord(int.min(from.row, row), int.min(from.col, col)),
            Coord(int.max(to.row, row), int.max(to.col, col)),
          )
        NoBox -> BoundingBox(coord, coord)
      }
      #([coord, ..areas], bounding_box)
    })
  let #(dijkstra, _) =
    areas
    |> list.fold(
      #(MultiDijkstra(dict.new(), set.new(), dict.new(), set.new()), 0),
      fn(acc, coord) {
        let #(dijkstra, id) = acc
        #(want_claim(dijkstra, id, coord), id + 1)
      },
    )
  let dijkstra = process_claims(dijkstra, from, to)
  let s1 =
    dict.fold(dijkstra.claim_count, 0, fn(best, id, count) {
      case set.contains(dijkstra.out_of_bounds, id) {
        True -> best
        False -> int.max(best, count)
      }
    })

  let midpoint =
    Coord(
      { to.row - from.row } / 2 + from.row,
      { to.col - from.col } / 2 + from.col,
    )
  let s2 = expand(areas, set.from_list([midpoint]), set.new()) |> set.size()

  #(int.to_string(s1), int.to_string(s2))
}

type BoundingBox {
  BoundingBox(from: Coord, to: Coord)
  NoBox
}

type MultiDijkstra {
  MultiDijkstra(
    wanted: Dict(Coord, Set(Int)),
    claimed: Set(Coord),
    claim_count: Dict(Int, Int),
    out_of_bounds: Set(Int),
  )
}

fn want_claim(dijkstra: MultiDijkstra, id: Int, coord: Coord) -> MultiDijkstra {
  MultiDijkstra(
    ..dijkstra,
    wanted: dict.upsert(dijkstra.wanted, coord, fn(x) {
      case x {
        Some(others) -> set.insert(others, id)
        None -> set.from_list([id])
      }
    }),
  )
}

fn process_claims(
  dijkstra: MultiDijkstra,
  from: Coord,
  to: Coord,
) -> MultiDijkstra {
  let wanted = dijkstra.wanted
  let dijkstra = MultiDijkstra(..dijkstra, wanted: dict.new())
  let dijkstra =
    dict.fold(wanted, dijkstra, fn(dijkstra, coord, ids) {
      let new_claimed = set.insert(dijkstra.claimed, coord)
      let does_claim_exist = set.contains(dijkstra.claimed, coord)
      let is_claim_out_of_bounds =
        coord.row < from.row
        || coord.col < from.col
        || coord.row > to.row
        || coord.col > to.col
      case does_claim_exist, set.to_list(ids), is_claim_out_of_bounds {
        False, [id], True ->
          MultiDijkstra(
            ..dijkstra,
            out_of_bounds: set.insert(dijkstra.out_of_bounds, id),
          )
        False, [id], False -> {
          let dijkstra =
            coord
            |> coord.manhattan()
            |> yielder.fold(dijkstra, fn(dijkstra, coord) {
              want_claim(dijkstra, id, coord)
            })
          let claim_count =
            dict.upsert(dijkstra.claim_count, id, fn(x) {
              case x {
                Some(count) -> count + 1
                None -> 1
              }
            })
          MultiDijkstra(
            ..dijkstra,
            claimed: new_claimed,
            claim_count: claim_count,
          )
        }
        _, _, _ -> MultiDijkstra(..dijkstra, claimed: new_claimed)
      }
    })
  case dict.size(dijkstra.wanted) {
    0 -> dijkstra
    _ -> process_claims(dijkstra, from, to)
  }
}

fn expand(
  areas: List(Coord),
  wanted: Set(Coord),
  claimed: Set(Coord),
) -> Set(Coord) {
  let #(wanted, claimed) =
    set.fold(wanted, #(set.new(), claimed), fn(acc, want) {
      let #(wanted, claimed) = acc
      case set.contains(claimed, want) {
        True -> acc
        False -> {
          let distance =
            list.try_fold(areas, 0, fn(acc, x) {
              let diff = coord.diff(x, want)
              case
                acc
                + int.absolute_value(diff.drow)
                + int.absolute_value(diff.dcol)
              {
                distance if distance >= 10_000 -> Error(Nil)
                distance -> Ok(distance)
              }
            })
          case distance {
            Error(_) -> acc
            Ok(_) -> {
              let wanted =
                want
                |> coord.manhattan()
                |> yielder.fold(wanted, fn(acc, coord) {
                  set.insert(acc, coord)
                })
              let claimed = set.insert(claimed, want)
              #(wanted, claimed)
            }
          }
        }
      }
    })
  case set.size(wanted) {
    0 -> claimed
    _ -> expand(areas, wanted, claimed)
  }
}
